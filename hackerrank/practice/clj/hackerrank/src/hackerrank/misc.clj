(ns hackerrank.misc
  (:require [clojure.string :as string]))

(defn rotate-string
  [s n] 
  (loop 
    [rs s i n res []] 
    (cond 
      (= i 0) res
      :else 
        (let [next-s (string/join "" (concat (drop 1 rs) (take 1 rs)))] 
          (recur next-s (dec i) (conj res next-s)))))) 

(defn remove-duplicates 
  [s]
  (loop [lst (vec s)
         res []
         dict #{}]
    (cond 
      (empty? lst) (println (string/join "" res))
      :else (cond 
              (contains? dict (first lst)) (recur (rest lst) res dict)
              :else (recur (rest lst) 
                           (conj res (first lst)) 
                           (conj dict (first lst)))))))

