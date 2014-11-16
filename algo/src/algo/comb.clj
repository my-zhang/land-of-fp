
(ns algo.comb) 

(defn perm [s] 
  "permuation of seq with distinct elements, 
  inspired by SICP." 
  (if (empty? s) 
    '(()) 
    (mapcat (fn [e] 
              (map #(cons e %) 
                   (perm (remove #(= e %) s))))
            s))) 

(defn comb [s] 
  (if (empty? s) 
    '(()) 
    (concat (comb (rest s)) 
            (map #(cons (first s) %) 
                 (comb (rest s)))))) 

