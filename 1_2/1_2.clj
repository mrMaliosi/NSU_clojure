(defn fletter
      "first letter"
      [alphabet cur]
      (if (= (str (last cur)) (first alphabet))
          (if (> (count alphabet) 1)
              (first (rest alphabet))
              `"")
          (first alphabet))
)

(defn ralph
      "rest alphabet"
      [alphabet cur]
      (if (= (str (last cur)) (first alphabet))
          (if (> (count alphabet) 1)
              (rest (rest alphabet))
              `())
          (rest alphabet))
)

(defn cycler
      "add letters"
      [alphabet cur ans]
      (if (or (= (count alphabet) 0) (and (= (count alphabet) 1) (= (str (last cur)) (first alphabet))))
          ans
          (recur (ralph alphabet cur) cur (concat ans (list (str cur (fletter alphabet cur))))))
)

(defn rast
      "ast with recur"
      [alphabet n ans]
      (if (= (count (first ans)) n)
          ans
          (recur alphabet n (rest (concat ans (cycler alphabet (first ans) `())))))
)

(defn ast
      "Builds and print AST tree len = n"
      [alphabet n cur cur_alphabet last_letter]
      (:pre [(>= n 0)])
      (if (< (count cur) n)
          (if (> (count cur_alphabet) 0)
              (if (not= (first cur_alphabet) last_letter)
                  (concat (ast alphabet n (str cur (first cur_alphabet)) alphabet (first cur_alphabet))
                          (ast alphabet n cur (rest cur_alphabet) last_letter))
                  (recur alphabet n cur (rest cur_alphabet) last_letter))
                  `())
              (list cur))
)

(defn all_str 
      "Return all possible strings len = n"
      [alphabet n]
      (:pre [(>= n 0)])
      (rast alphabet n `(""))
)

(println (all_str `("a" "b" "c") 10))
