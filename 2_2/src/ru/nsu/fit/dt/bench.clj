(ns ru.nsu.fit.dt.bench
  (:use ru.nsu.fit.dt.integral)
  (:gen-class))

(defn -main [& args]
  (let [h          0.01
        f          (fn [t] (Math/sin t)) ; интегрируемая функция sin(t)
        int-base   (integral f h)        ; базовая версия (без потоков)
        int-stream (integral-stream f h) ; оптимизированная версия (с потоками)
        ;; Сетка из 200 разных точек от 0.0 до 5.0 для имитации построения графика
        plot-xs    (range 0.0 5.0 0.025)]

    (println "=== БАЗА ===")
    (println "(x = 5.0, 1-й вызов):")
    (time (int-base 5.0))
    (println "(x = 5.0, 2-й вызов):")
    (time (int-base 5.0))
    (println "(x = 5.0, 3-й вызов):")
    (time (int-base 5.0))
    (println "(x = 4.5):")
    (time (int-base 4.5))
    (println "(x = 5.05):")
    (time (int-base 5.05))

    (println "\n=== ПОТОКИ (LAZY STREAMS) ===")
    (println "(x = 5.0, 1-й вызов):")
    (time (int-stream 5.0))
    (println "(x = 5.0, 2-й вызов):")
    (time (int-stream 5.0))
    (println "(x = 5.0, 3-й вызов):")
    (time (int-stream 5.0))
    (println "(x = 4.5 — берется готовый элемент из уже вычисленной части потока):")
    (time (int-stream 4.5))
    (println "(x = 5.05 — довычисляется только хвост потока от 5.0 до 5.05):")
    (time (int-stream 5.05))

    (println "\n=== Построение графика ===")
    ;; Создаем свежие экземпляры функций для честного сравнения
    (let [int-base-plot   (integral f h)
          int-stream-plot (integral-stream f h)]

      (println "Построение графика БЕЗ потоков:")
      (time (doall (map int-base-plot plot-xs)))

      (println "Построение графика С потоками (1-й проход по точкам):")
      (time (doall (map int-stream-plot plot-xs)))

      (println "Построение графика С потоками (2-й проход по тем же точкам):")
      (time (doall (map int-stream-plot plot-xs))))))
