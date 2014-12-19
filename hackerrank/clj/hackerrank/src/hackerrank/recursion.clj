(ns hackerrank.recursion
  (:require [clojure.string :as string]))

(defn pascal-triangle
  [n]
  (cond 
    (= n 1) 
      (do 
        (println 1)
        [1])
    :else 
      (let 
        [prev (pascal-triangle (dec n))
         row (vec (map + (cons 0 prev) (conj prev 0)))]
        (println (string/join " " row))
        row)))

(defn string-mingling 
  []
  (let 
    [p (read-line)
     q (read-line)]
    (println (string/join "" (interleave p q))))) 

(defn string-o-permute 
  [s] 
  (string/join " " (mapcat reverse (partition 2 s)))) 

(defn filter-elements 
  [lst q] 
  (->> lst 
       (filter (fn [num] (<= q (count (filter #(= num %) lst)))))
       (distinct))) 

