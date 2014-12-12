(ns hackerrank.misc
  (:require [clojure.string :as str]))

(defn rotate-string
  [s n] 
  (loop 
    [rs s i n res []] 
    (cond 
      (= i 0) res
      :else 
        (let [next-s (str/join "" (concat (drop 1 rs) (take 1 rs)))] 
          (recur next-s (dec i) (conj res next-s)))))) 

