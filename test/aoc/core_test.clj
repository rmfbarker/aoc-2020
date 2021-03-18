(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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

  (testing "parse batch file from disk and check for valid passports, checking the passport attributes as well as the required keys"
    (is (= 224
           (count-valid-passports
             (slurp
               (io/resource "input-day4"))))))

  (testing "passport attributes"

    ;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    ;cid (Country ID) - ignored, missing or not.

    (let [passport (parse-passport "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f")]
      (is (true? (valid-byr passport))))

    ;byr (Birth Year) - four digits; at least 1920 and at most 2002.
    (is (true? (valid-byr {"byr" "2002"})))
    (is (false? (valid-byr {"byr" "2003"})))

    ;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    (is (false? (valid-iyr {"iyr" "2002"})))
    (is (true? (valid-iyr {"iyr" "2010"})))

    ;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    (is (false? (valid-eyr {"eyr" "2019"})))
    (is (true? (valid-eyr {"eyr" "2030"})))


    ;hgt (Height) - a number followed by either cm or in:
    ;If cm, the number must be at least 150 and at most 193.
    ;If in, the number must be at least 59 and at most 76.

    ;hgt valid:   60in
    ;hgt valid:   190cm
    ;hgt invalid: 190in
    ;hgt invalid: 190
    (is (true? (valid-hgt {"hgt" "60in"})))
    (is (false? (valid-hgt {"hgt" "60cm"})))
    (is (true? (valid-hgt {"hgt" "190cm"})))
    (is (false? (valid-hgt {"hgt" "190in"})))
    (is (false? (valid-hgt {"hgt" "190"})))


    ;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ;hcl valid:   #123abc
    ;hcl invalid: #123abz
    ;hcl invalid: 123abc
    (is (true? (valid-hcl {"hcl" "#123abc"})))
    (is (false? (valid-hcl {"hcl" "#123abz"})))
    (is (false? (valid-hcl {"hcl" "123abc"})))
    (is (false? (valid-hcl {"hcl" "#z23abc"})))

    ;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.

    ;ecl valid:   brn
    ;ecl invalid: wat
    (is (true? (valid-ecl {"ecl" "brn"})))
    (is (false? (valid-ecl {"ecl" "wat"})))

    ;pid (Passport ID) - a nine-digit number, including leading zeroes.
    ;pid valid:   000000001
    ;pid invalid: 0123456789
    (is (true? (valid-pid {"pid" "000000001"})))
    (is (false? (valid-pid {"pid" "0123456789"})))


    ;Here are some invalid passports:
    ;
    ;eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
    (is (false? (valid-passport? "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926")))
    ;
    ;iyr:2019
    ;hcl:#602927 eyr:1967 hgt:170cm
    ;ecl:grn pid:012533040 byr:1946
    (is (false? (valid-passport? "iyr:2019\n    hcl:#602927 eyr:1967 hgt:170cm\n    ecl:grn pid:012533040 byr:1946\n")))

    ;hcl:dab227 iyr:2012
    ;ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
    (is (false? (valid-passport? "hcl:dab227 iyr:2012\n    ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n    ")))

    ;hgt:59cm ecl:zzz
    ;eyr:2038 hcl:74454a iyr:2023
    ;pid:3556412378 byr:2007
    (is (false? (valid-passport? "hgt:59cm ecl:zzz\n    eyr:2038 hcl:74454a iyr:2023\n    pid:3556412378 byr:2007")))


    ;Here are some valid passports:
    ;
    ;pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
    ;hcl:#623a2f
    (is (true? (valid-passport? "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n    hcl:#623a2f")))

    ;eyr:2029 ecl:blu cid:129 byr:1989
    ;iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
    (is (true? (valid-passport? "eyr:2029 ecl:blu cid:129 byr:1989\n    iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm")))

    ;hcl:#888785
    ;hgt:164cm byr:2001 iyr:2015 cid:88
    ;pid:545766238 ecl:hzl
    ;eyr:2022
    (is (true? (valid-passport? "hcl:#888785\n    hgt:164cm byr:2001 iyr:2015 cid:88\n    pid:545766238 ecl:hzl\n    eyr:2022")))

    ;iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
    (is (true? (valid-passport? "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")))))

(deftest day-7
  (is (= 238 (possible-outer-bags)))

  (let [example-bags
                   (parse-bags
                     (str/split-lines "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags."))

        day-7-bags (parse-bags (read-input "input-day7"))]
    (is (= 126 (get-bag-count example-bags "shiny gold")))
    (is (= 82930 (get-bag-count day-7-bags "shiny gold")))))

(deftest day-8
  (let [instructions (mapv parse-instruction (str/split-lines "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"))]
    (is (= 5 (second (accumulate instructions)))))

  (let [instructions (mapv parse-instruction (read-input "input-day8"))]
    (is (= 1489 (second (accumulate instructions)))))

  (is (= 1539 (fix-program))))

