(defn ast
      "Builds and print AST tree len = n"
      [alphabet n cur cur_alphabet last_letter]
      (:pre [(>= n 0)])
      (if (< (count cur) n)
          (if (> (count cur_alphabet) 0)
              (if (not= (first cur_alphabet) last_letter)
                  (concat (ast alphabet n (str cur (first cur_alphabet)) alphabet (first cur_alphabet))
                          (ast alphabet n cur (rest cur_alphabet) last_letter))
                  (ast alphabet n cur (rest cur_alphabet) last_letter))
              `())
              (list cur))
)

(defn all_str 
      "Return all possible strings len = n"
      [alphabet n]
      (:pre [(>= n 0)])
      (ast alphabet n `"" alphabet `"")
)

(println (all_str `("a" "b" "c") 2))
