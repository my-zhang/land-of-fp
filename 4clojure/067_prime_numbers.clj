; (= (__ 5) [2 3 5 7 11])

(fn [n] 
  "Return the first n prime numbers. 
  For each new number, check all the previous primes to see if there's any
  prime factor. Thank Naftali for the tip during interview Nov 14 2014." 
  (loop [i 2 r n lst []] 
    (cond 
     (= 0 r) lst 
     (some #(= 0 (mod i %)) lst) (recur (inc i) r lst) 
     :else (recur (inc i) (dec r) (conj lst i)))))

