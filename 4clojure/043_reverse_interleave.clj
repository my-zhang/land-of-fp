; (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
; (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

(fn [lst n] 
  (loop [i (dec n) res '()] 
    (if (< i 0) 
      res 
      (recur (dec i) 
             (conj res (map #(second %) 
                            (filter #(= i (mod (first %) n)) 
                                    (map-indexed vector lst))))))))

; OR 
(fn [lst n]
  "Amazing 'apply map'!" 
  (apply map vector (partition n lst)))

