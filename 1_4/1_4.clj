(defn ast
      "Builds and print AST tree len = n"
      [alphabet n]
      {:pre [(>= n 0)]}
      (reduce (fn [cur _]
                  (reduce (fn [acc s]
                              (concat acc 
                                      (map (fn [a]
                                               (str s a)
                                           )
                                           (filter (fn [a]
                                                       (not= (str (last s)) (str a)) 
                                                   )
                                                   alphabet)))
                          )
                          `()
                          cur)
              )
              `("")
              (range n))
)

(defn all_str 
      "Return all possible strings len = n"
      [alphabet n]
      {:pre [(>= n 0)]}
      (ast alphabet n)
)

(println (all_str `("a" "b" "c") 3))
