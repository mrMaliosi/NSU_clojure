(ns ru.nsu.fit.dt.integral-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [ru.nsu.fit.dt.integral :refer [integral integral-mem]]))

(def ^:const eps 1e-3)

(defn approx=
  "Проверка равенства чисел с заданной точностью eps"
  [a b]
  (< (Math/abs (- a b)) eps))

(deftest test-zero-point
  (testing "Интеграл с верхним пределом x = 0 должен давать 0.0"
    (let [f        (fn [t] (Math/sin t))
          int-base (integral f 0.01)
          int-mem  (integral-mem f 0.01)]
      (is (approx= 0.0 (int-base 0.0)))
      (is (approx= 0.0 (int-mem 0.0))))))

(deftest test-analytical-correctness
  (testing "Точность вычисления известного интеграла ∫_0^x sin(t) dt = 1 - cos(x)"
    (let [f        (fn [t] (Math/sin t))
          int-base (integral f 0.001)
          int-mem  (integral-mem f 0.001)]
      (doseq [x [0.5 1.0 2.5 5.0 -1.0]]
        (let [expected (- 1.0 (Math/cos x))]
          (is (approx= expected (int-base x)) (str "Ошибка в int-base при x = " x))
          (is (approx= expected (int-mem x)) (str "Ошибка в int-mem при x = " x)))))))

(deftest test-base-vs-mem-identity
  (testing "Совпадение результатов базовой и мемоизированной версий"
    (let [f        (fn [t] (+ (* 2.0 t) 1.0))
          int-base (integral f 0.01)
          int-mem  (integral-mem f 0.01)]
      (doseq [x [0.0 0.5 1.25 3.0 5.0 -2.0]]
        (is (approx= (int-base x) (int-mem x))
            (str "Несовпадение результатов для x = " x))))))

(deftest test-large-k-no-overflow
  (testing "Проверка отсутствия StackOverflowError при вычислении на большом количестве шагов"
    (let [f       (fn [t] (Math/sin t))
          int-mem (integral-mem f 0.001)]
      (is (number? (int-mem 50.0))))))

(deftest test-memoization-behavior
  (testing "Проверка, что f не вызывается повторно для кэшированных значений"
    (let [calls-count (atom 0)
          f           (fn [t]
                        (swap! calls-count inc)
                        (* t t))
          int-mem     (integral-mem f 0.01)]
      (int-mem 0.05)
      (let [initial-calls @calls-count]
        (is (> initial-calls 0) "Функция должна быть вызвана хотя бы раз")
        (int-mem 0.05)
        (is (= initial-calls @calls-count)
            "Повторный вызов int-mem не должен заново вычислять f для сетки")))))

(deftest test-default-arity
  (testing "Работа функций с параметрами по умолчанию (h = 0.01)"
    (let [f (fn [t] (* 2.0 t))]
      (is (approx= 1.0 ((integral f) 1.0)))
      (is (approx= 1.0 ((integral-mem f) 1.0))))))

(deftest test-non-integer-step-remainder
  (testing "Корректность расчета остатка (rem-area), когда x не кратно h"
    (let [f        (fn [t] (* 3.0 t t))
          h        0.01
          int-base (integral f h)
          int-mem  (integral-mem f h)
          x        0.255] ; не кратно h = 0.01
      (let [expected (Math/pow x 3)]
        (is (approx= expected (int-base x)))
        (is (approx= expected (int-mem x)))))))

(deftest test-exact-linear-integration
  (testing "Абсолютная точность метода трапеций на линейной функции f(t) = t"
    (let [f        identity
          int-base (integral f 0.1)
          int-mem  (integral-mem f 0.1)
          x        2.0
          expected 2.0]
      (is (< (Math/abs (- expected (int-base x))) 1e-9))
      (is (< (Math/abs (- expected (int-mem x))) 1e-9)))))

(defn -main [& args]
  (let [results (run-tests 'ru.nsu.fit.dt.integral-test)]
    (when (or (pos? (:fail results)) (pos? (:error results)))
      (System/exit 1))))
