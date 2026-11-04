(ns ru.nsu.fit.dt.integral
  (:require [clojure.test :as test])
  (:gen-class))

(defn integral
  "Базовая реализация интеграла с хвостовой рекурсией"
  ([f] (integral f 0.01))
  ([f h]
   (let [c (/ h 2.0)
         integral-k (fn [k]
                      (let [abs-k (Math/abs k)
                            sign  (if (neg? k) -1 1)]
                        (loop [curr 0
                               acc  0.0]
                          (if (= curr abs-k)
                            (* sign acc)
                            (let [f1    (f (* sign (* h curr)))
                                  f2    (f (* sign (* h (inc curr))))
                                  delta (* c (+ f1 f2))]
                              (recur (inc curr) (+ acc delta)))))))]
     (fn [x]
       (let [k        (int (quot x h))
             xk       (* k h)
             rem-area (* (+ (f xk) (f x))
                         (/ (- x xk) 2.0))]
         (+ (integral-k k) rem-area))))))

(defn integral-stream
  "Реализация интеграла с использованием бесконечных ленивых потоков"
  ([f] (integral-stream f 0.01))
  ([f h]
   (let [c (/ h 2.0)
         f-memo (memoize f)
         make-sums (fn [step]
                     (let [xs    (map #(* % step) (range))
                           fs    (map f-memo xs)
                           traps (map (fn [f1 f2] (* c (+ f1 f2)))
                                      fs
                                      (rest fs))]
                       (reductions + 0.0 traps)))
         pos-sums (make-sums h)
         neg-sums (make-sums (- h))]
     (fn [x]
       (let [k        (int (quot x h))
             xk       (* k h)
             base-sum (if (neg? k)
                        (- (nth neg-sums (Math/abs k)))
                        (nth pos-sums k))
             rem-area (if (zero? (- x xk))
                        0.0
                        (* (+ (f-memo xk) (f-memo x))
                           (/ (- x xk) 2.0)))]
         (+ base-sum rem-area))))))
