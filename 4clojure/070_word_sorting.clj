; (= (__  "Have a nice day.")
;   ["a" "day" "Have" "nice"])

(fn [s] 
  (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) 
        (re-seq #"\w+" s)))

; sort-by is recommended by doc. 
#(sort-by (fn [v](.toLowerCase v))  (re-seq #"\w+" %))

