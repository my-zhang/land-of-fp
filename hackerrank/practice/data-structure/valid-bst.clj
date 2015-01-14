
(require '[clojure.string :as string])

(defn sub-order 
  [pre] 
  (let 
    [root (first pre)
     left (take-while #(> root %) (rest pre)) 
     right (drop-while #(> root %) (rest pre))] 

    [left right])) 

(defn valid-bst 
  [preorder] 
  (cond 
    (>= 1 (count preorder)) true 
    :else 
      (let 
        [root (first preorder) 
         [left right] (sub-order preorder)] 
        (cond 
          (and (not (empty? left))
               (< root (apply max left))) 
            false 
          (and (not (empty? right)) 
               (> root (apply min right)))
            false 
          :else 
            (and (valid-bst left) (valid-bst right))))))   

(let 
  [T (read-string (read-line))]
  (doseq [t (range T)] 
    (let 
      [n (read-string (read-line)) 
       preorder (map #(Integer/parseInt %) 
                     (string/split (read-line) #"\s+"))] 
      (if (valid-bst preorder) 
        (println "YES") 
        (println "NO"))))) 
