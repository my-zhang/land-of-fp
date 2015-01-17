(require '[clojure.string :as string])

(defn next-states 
  [r1 r2 r3] 
  (concat 
    (for [i (range r1)] [i (min i r2) (min i r3)]) 
    (for [i (range r2)] [r1 i (min i r3)]) 
    (for [i (range r3)] [r1 r2 i])))

(def succuess-state? 
  (memoize 
    (fn f [r1 r2 r3] 
      (cond 
        (= [r1 r2 r3] [1 0 0]) false 
        (= [r1 r2 r3] [0 0 0]) true 
        :else 
          (if (every? true? (map #(apply succuess-state? %)
                                 (next-states r1 r2 r3))) 
            false 
            true))))) 

(let 
  [T (read-string (read-line))] 
    (doseq [t (range T)
            :let [[r1 r2 r3] (map #(Integer/parseInt %) 
                            (string/split (read-line) #"\s+"))]]
      (if (succuess-state? r1 r2 r3) 
        (println "WIN") 
        (println "LOSE"))))
