(ns hackerrank.recursion-test
  (:require [clojure.test :refer :all]
            [hackerrank.recursion :refer :all]))

(deftest pascal-triangle-test
  (testing "pascal-triangle"
    (pascal-triangle 4)))
