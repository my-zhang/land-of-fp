
(def M 100000007)

(def num-of-bst 
  (memoize 
    (fn f [n] 
      (cond 
        (= 0 n) 1 
        (= 1 n) 1 
        :else 
          (loop [l 0 cnt 0]
            (if (= l n) 
              cnt 
              (recur (inc l) 
                     (mod (+ cnt 
                             (* (num-of-bst l) 
                                (num-of-bst (- n 1 l))))
                          M))))))))

(let 
  [T (read-string (read-line))] 

  (doseq [t (range T)] 
    (let 
      [n (read-string (read-line))] 
      (println (num-of-bst n))))) 
