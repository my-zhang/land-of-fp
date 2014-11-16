
; (= true (__ 11))
; (= true (__ 89098))

(fn [number] 
  (let [s (str number)
        n (count s)
        l (take (quot (inc n) 2) s)
        r (drop (quot n 2) s)] 
    (= (apply + (map #(Integer/parseInt (str %)) l))
 	   (apply + (map #(Integer/parseInt (str %)) r)))))
