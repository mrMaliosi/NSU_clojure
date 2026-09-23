(defn my-map
      "My own map realization for one collection"
      [f collection]
      (reduce (fn [acc item] 
                  (conj acc (f item)))
                  [] 
                  collection
              )
)

(defn my-filter
      "My own filter realization for one collection"
      [f col]
      (reduce (fn [acc item]
                  (if (f item)
                    (conj acc item)
                    acc))
               []
               col)
)

(println (my-map inc [1 2 3]))
(my-filter even? [1 2 3 4 5 6])
