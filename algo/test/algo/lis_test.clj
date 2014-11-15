(ns algo.lis-test
  (:require [clojure.test :refer :all]
            [algo.lis :refer :all]))

(deftest dp-test
  (testing "Longest Increasing Subsequnce."
    (is (= [3] (dp '(3))))
    (is (= [1 3] (dp '(1 3))))
    (is (= [0 4 6 9 13 15] (dp '(0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15))))
    ))

