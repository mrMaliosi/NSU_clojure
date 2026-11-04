(ns filter.impl)

;; -----------------------------------------------------------------------------
;; Вспомогательные функции
;; -----------------------------------------------------------------------------

;; Количество доступных ядер процессора
(def n-cpu (.availableProcessors (Runtime/getRuntime)))

;; Бесконечный поток натуральных чисел
(def naturals
  (lazy-seq
    (cons 1 (map inc naturals))))

;; Тяжелый предикат для имитации вычислительной нагрузки
(defn heavy-pred [x]
  (Thread/sleep 1)
  (even? x))

(defn -partition
  "Ручное разбиение коллекции на блоки заданного размера."
  [n coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (cons (doall (take n s))
            (-partition n (drop n s))))))

;; -----------------------------------------------------------------------------
;; Задача 3.1: Строгий параллельный pfilter (Eager)
;; -----------------------------------------------------------------------------

(defn pfilter-eager
  "Параллельный неленивый filter для конечных коллекций.
   Разбивает coll на блоки и обрабатывает их параллельно через future."
  ([pred coll] 
   (pfilter-eager pred coll 32))
  ([pred coll block-size]
   (let [chunks  (-partition block-size coll)
         futures (doall (map #(future (doall (filter pred %))) chunks))]
     (doall (mapcat deref futures)))))    ;; deref - необходимая блокировка основного потока до завершения future

;; -----------------------------------------------------------------------------
;; Задача 3.2: Ленивый параллельный pfilter (Lazy / Sliding Window)
;; -----------------------------------------------------------------------------

(defn pfilter-lazy
  "Ленивый параллельный filter, поддерживающий бесконечные потоки.
   Использует скользящее окно из n-threads активных future."
  ([pred coll] 
   (pfilter-lazy pred coll 32))
  ([pred coll block-size] 
   (pfilter-lazy pred coll block-size n-cpu))
  ([pred coll block-size n-threads]
   (let [chunks (-partition block-size coll)]
     (letfn [(step [remaining-chunks active-futures]
               (lazy-seq
                 (when (seq active-futures)
                   (let [first-fut       (first active-futures)
                         filtered-chunk  @first-fut
                         more-chunks     (rest remaining-chunks)
                         ;; Подгружаем следующий блок только по мере необходимости
                         next-fut        (when (seq remaining-chunks)
                                           (future (doall (filter pred (first remaining-chunks)))))
                         new-active      (cond-> (subvec (vec active-futures) 1)
                                           next-fut (conj next-fut))]
                     (concat filtered-chunk (step more-chunks new-active))))))]
       (let [initial-chunks  (take n-threads chunks)
             rest-chunks     (drop n-threads chunks)
             initial-futures (mapv #(future (doall (filter pred %))) initial-chunks)]
         (step rest-chunks initial-futures))))))

;; -----------------------------------------------------------------------------
;; Единый публичный API
;; -----------------------------------------------------------------------------

(defn pfilter
  "Универсальный ленивый параллельный filter (по умолчанию блок = 32)."
  ([pred coll] 
   (pfilter-lazy pred coll 32))
  ([pred coll block-size] 
   (pfilter-lazy pred coll block-size)))
