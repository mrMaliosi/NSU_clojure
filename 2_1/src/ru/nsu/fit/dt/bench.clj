(ns ru.nsu.fit.dt.benchmark
  (:use ru.nsu.fit.dt.integral)
  (:gen-class))

(defn -main [& args]
  (let [h         0.01
        f         (fn [t] (Math/sin t)) ; интегрируемая функция sin(t)
        int-base  (integral f h)        ; базовая версия (без мемоизации)
        int-mem   (integral-mem f h)    ; оптимизированная версия (с мемоизацией)
        ;; Сетка из 200 разных точек от 0.0 до 5.0 для имитации построения графика
        plot-xs   (range 0.0 5.0 0.025)]

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

    (println "\n=== КЭШ ===")
    (println "(x = 5.0, 1-й вызов:")
    (time (int-mem 5.0))
    (println "(x = 5.0, 2-й вызов):")
    (time (int-mem 5.0))
    (println "(x = 5.0, 3-й вызов):")
    (time (int-mem 5.0))    
    (println "(x = 4.5):")
    (time (int-mem 4.5))
    (println "(x = 5.05):")
    (time (int-mem 5.05))

    (println "\n=== Построение графика ===")
    ;; Создаем свежие экземпляры функций с чистым кэшем для честного сравнения
    (let [int-base-plot (integral f h)
          int-mem-plot  (integral-mem f h)]

      (println "Построение графика БЕЗ мемоизации:")
      (time (doall (map int-base-plot plot-xs)))

      (println "Построение графика С мемоизацией (1-й проход по точкам):")
      (time (doall (map int-mem-plot plot-xs)))

      (println "Построение графика С мемоизацией (2-й проход по тем же точкам):")
      (time (doall (map int-mem-plot plot-xs))))))

(-main)
