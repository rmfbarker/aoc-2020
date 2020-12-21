(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest day-1
  (testing "example given"
    (is (= 514579
           (calc-day-1 [1721
                        979
                        366
                        299
                        675
                        1456])))))
