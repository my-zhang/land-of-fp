; RESRICTION frequencies

; (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})

(fn [lst] 
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} lst)) 

