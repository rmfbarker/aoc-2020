(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest day-1
  (testing "basic example"
    (is (= 514579
           (calc-day-1 [1721
                        979
                        366
                        299
                        675
                        1456])))

    (is (= 241861950
           (calc-day-1-part-2 [1721
                               979
                               366
                               299
                               675
                               1456]))))

  (testing "question 1"
    (is (= 355875
           (calc-day-1 input-day1))))

  (testing "question 2"
    (is (= 140379120
           (calc-day-1-part-2 input-day1)))))
