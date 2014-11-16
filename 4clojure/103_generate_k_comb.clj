; (= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}})
; (= (__ 10 #{4 5 6}) #{})
; (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})

(fn k-comb [k s] 
  (if (empty? s) 
    '(()) 
    (set (map set 
              (filter #(= k (count %)) 
                      (concat (k-comb k (rest s)) 
                              (map #(cons (first s) %) 
                                   (k-comb (dec k) (rest s)))))))))

