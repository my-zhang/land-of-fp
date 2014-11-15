; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
; (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
; (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))

(fn [r lst] 
  "Notice the diff between 'mod' and 'rem'." 
  (let [shift (mod r (count lst))] 
    (concat (drop shift lst) (take shift lst))))

