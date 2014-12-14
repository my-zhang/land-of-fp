(ns algo.nqueen)

(defn valid-pattern? 
  "check if pattern compatible with last row."
  [pattern] 
  (let 
    [k (count pattern)
     pos (last pattern)
     points (map vector 
                 (range) (take (dec k) pattern))
     non-conflict? (fn [r1 c1 r2 c2] 
                     (and (not= c1 c2) 
                          (not= (- r1 c1) (- r2 c2))
                          (not= (+ r1 c1) (+ r2 c2))))] 
    (every? #(apply non-conflict? %)
            (map concat points (repeat [(dec k) pos])))))

(defn queens
  "all valid board patterns of first k rows of n-queen."
  [n k]
  (cond 
    (= k 1) (map vector (range n)) 
    :else 
      (filter valid-pattern? 
              (mapcat (fn [pattern] (map #(conj pattern %)
                                      (range n)))
                      (queens n (dec k)))))) 

