(ns ru.nsu.fit.dt.integral-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [ru.nsu.fit.dt.integral :refer [integral integral-stream]]))

(def ^:const eps 1e-3)

(defn approx=
  "Проверка равенства чисел с заданной точностью eps"
  [a b]
  (< (Math/abs (- a b)) eps))

(deftest test-zero-point
  (testing "Интеграл с верхним пределом x = 0 должен давать 0.0"
    (let [f          (fn [t] (Math/sin t))
          int-base   (integral f 0.01)
          int-stream (integral-stream f 0.01)]
      (is (approx= 0.0 (int-base 0.0)))
      (is (approx= 0.0 (int-stream 0.0))))))

(deftest test-analytical-correctness
  (testing "Точность вычисления известного интеграла ∫_0^x sin(t) dt = 1 - cos(x)"
    (let [f          (fn [t] (Math/sin t))
          int-base   (integral f 0.001)
          int-stream (integral-stream f 0.001)]
      (doseq [x [0.5 1.0 2.5 5.0 -1.0]]
        (let [expected (- 1.0 (Math/cos x))]
          (is (approx= expected (int-base x)) (str "Ошибка в int-base при x = " x))
          (is (approx= expected (int-stream x)) (str "Ошибка в int-stream при x = " x)))))))

(deftest test-base-vs-stream-identity
  (testing "Совпадение результатов базовой и потоковой версий"
    (let [f          (fn [t] (+ (* 2.0 t) 1.0))
          int-base   (integral f 0.01)
          int-stream (integral-stream f 0.01)]
      (doseq [x [0.0 0.5 1.25 3.0 5.0 -2.0]]
        (is (approx= (int-base x) (int-stream x))
            (str "Несовпадение результатов для x = " x))))))

(deftest test-large-k-no-overflow
  (testing "Проверка отсутствия StackOverflowError при вычислении на большом количестве шагов"
    (let [f          (fn [t] (Math/sin t))
          int-stream (integral-stream f 0.001)]
      (is (number? (int-stream 50.0))))))

(deftest test-stream-caching-behavior
  (testing "Проверка, что ленивый поток кэширует вычисленные элементы и не вызывает f повторно"
    (let [calls-count (atom 0)
          f           (fn [t]
                        (swap! calls-count inc)
                        (* t t))
          int-stream  (integral-stream f 0.01)]
      (int-stream 0.05)
      (let [initial-calls @calls-count]
        (is (> initial-calls 0) "Функция должна быть вызвана хотя бы раз")
        ;; Повторный вызов в той же точке
        (int-stream 0.05)
        (is (= initial-calls @calls-count)
            "Повторный вызов int-stream в 0.05 не должен заново вычислять f")
        ;; Вызов в меньшей точке (уже пройденной в потоке)
        (int-stream 0.03)
        (is (= initial-calls @calls-count)
            "Вызов int-stream для уже пройденного узла сетки (0.03) не должен вызывать f")))))

(deftest test-default-arity
  (testing "Работа функций с параметрами по умолчанию (h = 0.01)"
    (let [f (fn [t] (* 2.0 t))]
      (is (approx= 1.0 ((integral f) 1.0)))
      (is (approx= 1.0 ((integral-stream f) 1.0))))))

(deftest test-non-integer-step-remainder
  (testing "Корректность расчета остатка (rem-area), когда x не кратно h"
    (let [f          (fn [t] (* 3.0 t t))
          h          0.01
          int-base   (integral f h)
          int-stream (integral-stream f h)
          x          0.255] ; не кратно h = 0.01
      (let [expected (Math/pow x 3)]
        (is (approx= expected (int-base x)))
        (is (approx= expected (int-stream x)))))))

(deftest test-exact-linear-integration
  (testing "Абсолютная точность метода трапеций на линейной функции f(t) = t"
    (let [f          identity
          int-base   (integral f 0.1)
          int-stream (integral-stream f 0.1)
          x          2.0
          expected   2.0]
      (is (< (Math/abs (- expected (int-base x))) 1e-9))
      (is (< (Math/abs (- expected (int-stream x))) 1e-9)))))

(defn -main [& args]
  (let [results (run-tests 'ru.nsu.fit.dt.integral-test)]
    (when (or (pos? (:fail results)) (pos? (:error results)))
      (System/exit 1))))
