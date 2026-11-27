(ns ru.nsu.fit.dt.integral
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

(defn integral-mem
    "Базовая реализация интегралла"
    ([f] (integral-mem f 0.01))
    ([f h]
        (let [c     (/ h 2.0)
              f-mem (memoize f)
              integral-k (memoize
                      (fn [k]
                          (let [abs-k (Math/abs k)
                                sign  (if (neg? k) -1 1)]
                               (loop [curr 0
                                      acc  0.0]
                                    (if (= curr abs-k)
                                    (* sign acc)
                                    (let [f1    (f-mem (* sign (* h curr)))
                                          f2    (f-mem (* sign (* h (inc curr))))
                                          delta (* c (+ f1 f2))]
                                          (recur (inc curr) (+ acc delta))))))))]
        (fn [x]
            (let [k        (int (quot x h))
                  xk       (* k h)
                  rem-area (* (+ (f-mem xk) (f-mem x))
                              (/ (- x xk) 2.0))]
                 (+ (integral-k k) rem-area))))))
