
(require '[clojure.string :as string]) 

(def M 1000000007) 

(def primes '(2 3 5 7 11 13 17 19 23 29 31 
                37 41 43 47 53 59 61 67 71 73 79 83 89 97)) 
(defn mod-pow
  "Returns the remainder of b raised to the e-th power 
  when divided by m."
  [p e m]
  (loop [accu 1 
         a p 
         b e] 
    (if (<= b 0) 
      accu 
      (if (= (bit-and b 1) 1)
        (recur (mod (* accu a) m) (mod (* a a) m) (bit-shift-right b 1)) 
        (recur accu (mod (* a a) m) (bit-shift-right b 1))))))

(defn val-to-vec 
  [n]
  (for [p primes]
    (if (= 0 (mod n p)) 
      (loop [i n e 0]
        (if (= 0 (mod i p)) 
          (recur (quot i p) (inc e)) 
          e))
      0))) 

(defn vec-value 
  [v]
  (reduce (fn [a b] (mod (* a b) M)) 
          (map (fn [p e] (mod-pow p e M)) primes v))) 

(defn vec-mul 
  [v u] 
  (map (fn [i j] (+ i j)) v u)) 

(defn vec-lcm
  [v u] 
  (map (fn [i j] (max i j)) v u)) 

(defn mid 
  [l r] 
  (bit-shift-right (+ l r) 1)) 

(definterface INode 
  (get_left []) 
  (get_right [])
  (get_val [])
  (set_left [node])
  (set_right [node])
  (set_val [v])
  (insert_val [i v])
  (query_segs [i j])
  (query [i j])
  (preorder_print [])
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
  (insert_val [this i v] 
    (if (= l r) 
      (.set_val this v) 
      (do 
        (if (= 0 (.get_val this)) 
          (.set_val this v) 
          (.set_val this (vec-lcm (.get_val this) v))) 

        (if (<= i (mid l r)) 
          (.insert_val left i v) 
          (.insert_val right i v)))))

  (query_segs [this i j] 
    (cond 
      (and (= i l) (= j r)) (vector val)
      (<= j (mid l r)) (.query_segs left i j) 
      (> i (mid l r)) (.query_segs right i j) 
      :else 
        (concat (.query_segs left i (mid l r)) 
                (.query_segs right (inc (mid l r)) j)))) 
    
  (query [this i j] 
    (let 
      [segs (.query_segs this i j)]
      ;; (println "segs" i j segs)
      (if (= 1 (count segs)) 
        (first segs)
        (reduce vec-lcm segs))))

  (preorder_print [_] 
    (println "[" l r "]" val) 
    (if left
      (.preorder_print left) 
      nil) 
    (if right 
      (.preorder_print right) 
      nil)) 
  ) 

(defn build-tree 
  [l r] 
  (let 
    [root (Node. l r 0 nil nil)]
    (if (= l r) 
      root
      (do 
        (.set_left root (build-tree l (mid l r))) 
        (.set_right root (build-tree (inc (mid l r)) r))
        root))))
      
(let 
  [n (Integer/parseInt (read-line)) 
   nums (map #(Integer/parseInt %) 
             (string/split (read-line) #"\s+"))
   m (Integer/parseInt (read-line))]

  (let 
    [root (build-tree 0 (dec n))] 
    (doseq [[i v] (map vector (range) nums)
            :let [vlst (val-to-vec v)]] 
      (.insert_val root i vlst)) 
    ;; (.preorder_print root)
    (doseq [t (range m)
            :let [[cmd x y] (map read-string 
                                 (string/split (read-line) #"\s+"))]]
      (cond 
        (= 'Q cmd) (println (vec-value (.query root x y))) 
        (= 'U cmd) (.insert_val root 
                                x 
                                (vec-mul (.query root x x) 
                                         (val-to-vec y)))
        :else (throw (Exception. "unknown cmd type")))))) 

