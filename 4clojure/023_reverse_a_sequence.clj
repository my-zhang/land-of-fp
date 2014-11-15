; (= (__ [1 2 3 4 5]) [5 4 3 2 1])

(fn [lst] 
  (loop [l lst res '()] 
    (if (empty? l) 
      res
      (recur (rest l) (cons (first l) res)))))

; A BETTER WAY 

reduce conj '()

