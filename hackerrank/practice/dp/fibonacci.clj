
(def M 100000007) 

(def fib
  (memoize 
    (fn [n] 
      (cond 
        (= 0 n) 0 
        (= 1 n) 1 
        :else 
          (let 
            [a (fib (- n 1))
             b (fib (- n 2))]
            (if (>= (+ a b) M) 
              (- (+ a b) M) 
              (+ a b))))))) 
          
(let 
  [T (read-string (read-line))]
  (doseq [t (range T)] 
    (let 
      [n (read-string (read-line))] 
      (println (fib n))))) 
