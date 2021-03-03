(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]
            [clojure.java.io :as io]))

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


(deftest day4
  (testing "valid passport"
    (is (true? (valid-passport? "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm")))
    (is (true? (valid-passport? "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm"))))

  (testing "invalid passport"
    (is (false? (valid-passport? "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929")))
    (is (false? (valid-passport? "foo:gry bar:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm")))
    (is (false? (valid-passport? "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"))))

  (testing "parse batch"
    (is (= 2 (count-valid-passports "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"))))

  (testing "parse batch file from disk"
    (is (= 264
           (count-valid-passports
             (slurp
               (io/resource "input-day4"))))))
  )