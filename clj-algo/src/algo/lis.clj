(ns algo.lis) 

(defn longest-seq [seq-col] 
  "return the longest seq in the collection, 
  when multiple answers return the first one.

  'max-key count' does the same,
  expcet for the orderness of multiple answers." 
  (let 
    [mseq (apply max-key count seq-col)] 
    (first (filter #(= (count mseq) (count %)) seq-col)))) 

(defn dp [lst] 
  (if (= 1 (count lst)) 
    (vec lst) 
    (let 
      [subproblems (for [i (range 1 (count lst))
                         :let [inc-lst (dp (take i lst))]
                         :when (> (last lst) (last inc-lst))]
                     inc-lst)] 
      (if (empty? subproblems) 
        (vector (last lst)) 
        (conj (longest-seq subproblems) (last lst)))))) 
 
(defn longest-inc-subseq [lst] 
  "return the LIS of the given seq, 
  if multiply answers, return the first occurence." 
  (let 
    [dp (memoize dp)
     subproblems (for [i (range 1 (count lst))] (dp (take i lst)))] 
    (longest-seq subproblems))) 

