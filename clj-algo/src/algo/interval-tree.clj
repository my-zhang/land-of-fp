(ns algo.interval-tree
  :require '[clojure.string :as string])) 

(definterface INode 
  (get_left []) 
  (get_right [])
  (get_val [])
  (set_left [node])
  (set_right [node])
  (set_val [v])
  (mid []) 
  (insert_val [i v])
  (query_segs [i j])
  (query [i j])
  ) 

(deftype Node 
  [l
   r 
   ^:volatile-mutable val 
   ^:volatile-mutable ^INode left
   ^:volatile-mutable ^INode right] 

  INode
  (get_left [_] left) 
  (get_right [_] right)
  (get_val [_] val)
  (set_left [this node] (set! left node))
  (set_right [this node] (set! right node)) 
  (set_val [this v] (set! val v)) 

  (mid [_] 
   (bit-shift-right (+ l r) 1))

  (insert_val [this i v] 
    (if (= l r) 
      (.set_val this v) 
      (do 
        (.set_val this (max v (.get_val this))) 

        (if (<= i (.mid this)) 
          (.insert_val left i v) 
          (.insert_val right i v)))))

  (query_segs [this i j] 
    "return optimal values of sub intervals of [i j]."
    (cond 
      (and (= i l) (= j r)) (vector val)
      (<= j (.mid this)) (.query_segs left i j) 
      (> i (.mid this)) (.query_segs right i j) 
      :else 
        (concat (.query_segs left i (.mid this)) 
                (.query_segs right (inc (.mid this)) j)))) 
    
  (query [this i j] 
    (let 
      [segs (.query_segs this i j)]
      (reduce max segs)))

  ) 

(defn build-tree 
  "Build the structure of interval tree of [l r], 
  assign 0 to each node as default value. " 
  [l r] 
  (let 
    [root (Node. l r 0 nil nil)]
    (if (= l r) 
      root
      (do 
        (.set_left root (build-tree l (.mid root))) 
        (.set_right root (build-tree (inc (.mid root)) r))
        root))))
      
