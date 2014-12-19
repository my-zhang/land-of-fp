(ns hackerrank.data-structure
  (:require [clojure.string :as string]))

(defn build-prev-dict 
  [n m mat] 
  (let 
    [prev-index (fn [i j ra rb ca cb] 
                    (cond 
                      (and (= i ra) (< j cb)) [[i j] [i (inc j)]] 
                      (and (= i rb) (> j ca)) [[i j] [i (dec j)]] 
                      (and (= j ca) (> i ra)) [[i j] [(dec i) j]] 
                      (and (= j cb) (< i rb)) [[i j] [(inc i) j]]))] 
    (into {} 
          (for [i (range n) j (range m)
                :let [layer (min i j (- n i 1) (- m j 1))]]
            (prev-index i j layer (- n layer 1) layer (- m layer 1)))))) 

(defn rotate-prev-dict 
  [n m d r] 
  (loop [D (into {} (for [i (range n) j (range m)] 
                      [[i j] [i j]]))
         k r] 
    (cond 
      (= k 0) D 
      :else 
        (recur (into {} 
                     (for [i (range n) j (range m)] 
                       [[i j] (d (D [i j]))]))
               (dec k))))) 

(defn matrix-rotation 
  [n m r mat] 
  (let 
    [D (rotate-prev-dict n m (build-prev-dict n m mat) r)] 
    (doseq [i (range n) j (range m)
          :let [[r c] (D [i j])]]
      (do 
        (print (nth (nth mat r) c))
        (cond 
          (= j (dec m)) (println)
          :else (print " "))))))

