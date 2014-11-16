; (= :x (__ [[:x :e :o]
;            [:x :e :e]
;            [:x :e :o]]))

; (= nil (__ [[:x :o :x]
;             [:x :o :x]
;             [:o :x :o]]))

(fn [g] 
  (let [[r1 r2 r3] g
        cols (apply map vector g)
        d1 [(first r1) (second r2) (last r3)]
        d2 [(last r1) (second r2) (first r3)]
        patterns (concat g cols [d1 d2])]
    (cond 
     (some #(= '(:x :x :x) %) patterns) :x 
     (some #(= '(:o :o :o) %) patterns) :o
     :else nil)))

