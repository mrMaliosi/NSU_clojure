(ns filter.tests
  (:require [clojure.test :refer [deftest is run-tests]]
            [filter.impl :refer [pfilter pfilter-eager pfilter-lazy]]
            [filter.impl :refer [naturals heavy-pred]]))

(deftest test-range
  (doseq [[pred max-range] [[even? 1]
                            [odd? 1]
                            [even? 2]
                            [even? 3]
                            [even? 4]
                            [even? 5]
                            [even? 100]
                            [(fn [_] true) 1000]]]
    (let [coll     (range max-range)
          expected (filter pred coll)]
      (is (= expected (pfilter-eager pred coll 10)) "Ошибка в pfilter-eager")
      (is (= expected (pfilter-lazy pred coll 10))  "Ошибка в pfilter-lazy")
      (is (= expected (pfilter pred coll 10))       "Ошибка в pfilter API"))))

(deftest test-naturals
  (let [pred     (fn [x] (even? x))
        expected (filter pred naturals)
        actual   (pfilter pred naturals 10)]
    (doseq [n [0 1 2 5 10 100 1000 10000]]
      (is (= (nth expected n) (nth actual n)) 
          (str "Несовпадение на элементе " n)))))

(deftest run-benchmarks
  (println "\n--------------BENCHMARK--------------")
  (let [r          (range 400)
        block-size 32]
    (print "1. Последовательный filter:        ")
    (time (doall (filter heavy-pred r)))
    
    (print "2. Параллельный pfilter-eager (3.1): ")
    (time (pfilter-eager heavy-pred r block-size))
    
    (print "3. Параллельный pfilter-lazy (3.2):  ")
    (time (doall (take 200 (pfilter-lazy heavy-pred r block-size))))))

;; Запуск тестов
(defn -main [& args]
  (run-tests 'filter.tests)
  (System/exit 0))
