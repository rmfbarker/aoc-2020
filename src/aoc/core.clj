(ns aoc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn calc-day-1 [input]
  (first
    (for [x input
          y input
          :let [sum     (+ x y)
                product (* x y)]
          :when (and (not= x y)
                     (= 2020 sum))]
      product)))

(defn calc-day-1-part-2 [input]
  (first
    (for [x input
          y input
          z input
          :let [sum     (+ x y z)
                product (* x y z)]
          :when (and (not= x y z)
                     (= 2020 sum))]
      product)))

(defn read-input [filename]
  (clojure.string/split-lines
    (slurp
      (io/resource filename))))

(def input-day1 (map #(Integer/parseInt %)
                     (read-input "input-day1")))

(def answer-1 (calc-day-1 input-day1))
(def answer-2 (calc-day-1-part-2 input-day1))

;; Day 2

(def answer-1 (count
                (filter
                  (fn [pwd-policy]
                    (let [
                          [character-policy [character _] password] (str/split pwd-policy #" ")
                          [lwr uppr] (str/split character-policy #"-")
                          character-count (count (filter #{character} password))
                          lwr             (Integer/parseInt lwr)
                          uppr            (Integer/parseInt uppr)]

                      (<= lwr character-count uppr)))

                  (read-input "input-day2"))))

(def answer-2 (count
                (filter
                  (fn [pwd-policy]
                    (let [[character-policy [character _] password] (str/split pwd-policy #" ")
                          [char-1 char-2] (map #(get password (dec (Integer/parseInt %)))
                                               (str/split character-policy #"-"))
                          valid (= 1 (count (filter identity [(= character char-1) (= character char-2)])))]
                      valid))
                  (read-input "input-day2"))))

;; Day 3

(def day3-basic-example (str/split-lines "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"))

(defn get-at [grid [x y]]
  (if-let [row (get grid y)]
    (let [x-in (mod x (count row))]
      (get row x-in))))

(defn get-steps [step-right step-down]
  (let [start-pos [0 0]]
    (iterate
      (fn [[x y]]
        [(+ x step-right) (+ y step-down)])
      start-pos)))

(defn count-trees [grid angle]
  (let [[step-right step-down] angle]
    (count
      (filter
        #(= \# %)
        (take-while identity
                    (map (partial get-at grid)
                         (get-steps step-right step-down)))))))

(def answer-3
  (count-trees (read-input "input-day3") [3 1]))

(def angles [{:Right 1, :down 1}
             {:Right 3, :down 1}
             {:Right 5, :down 1}
             {:Right 7, :down 1}
             {:Right 1, :down 2}])

(def answer-3-b
  (reduce *
          (let [grid (read-input "input-day3")]
            (map (fn [{:keys [Right down]}]
                   (count-trees grid [Right down]))
                 angles))))


(defn parse-passport [passport]
  (into
    {}
    (map
      #(clojure.string/split % #":")
      (clojure.string/split (clojure.string/trim passport) #"[ \t\n]+"))))

(defn parse-passports [batch-file]
  (clojure.string/split batch-file #"\n\n"))

(let [passport   "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f"
      birth-year (get (parse-passport passport) "byr")]
  (when birth-year
    (and (re-find #"\d{4}" birth-year)
         (<= 1920 (Integer/parseInt birth-year) 2002))))

(defn valid-yr [attr lwr uppr]
  (when attr
    (and (re-find #"\d{4}" attr)
         (<= lwr (Integer/parseInt attr) uppr))))


;byr (Birth Year) - four digits; at least 1920 and at most 2002.
;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;hgt (Height) - a number followed by either cm or in:
;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.
;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;pid (Passport ID) - a nine-digit number, including leading zeroes.
;cid (Country ID) - ignored, missing or not.

(defn valid-byr [passport]
  (let [birth-year (get passport "byr")]
    (valid-yr birth-year 1920 2002)))

(defn valid-iyr [passport]
  (let [year (get passport "iyr")]
    (valid-yr year 2010 2020)))

(defn valid-eyr [passport]
  (let [year (get passport "eyr")]
    (valid-yr year 2020 2030)))

(defn parse-int [s]
  (try (Integer/parseInt s)
       (catch NumberFormatException e)))

(defn valid-hgt [passport]
  (let [hgt  (get passport "hgt")
        unit (re-find #"cm$|in$" hgt)
        size (parse-int (re-find #"\d+" hgt))]
    (and
      (some? (re-find #"\d+in$|\d+cm$" hgt))
      (if (= "cm" unit)
        (<= 150 size 193)
        (<= 59 size 76)))))

(defn valid-hcl [passport]
  (let [hcl (get passport "hcl")]
    (and hcl
         (some? (re-find #"#[0-9a-f]{6}" hcl)))))

(defn valid-ecl [passport]
  (let [ecl (get passport "ecl")]
    (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)))

(defn valid-pid [passport]
  (let [pid (get passport "pid")]
    (and pid (some? (re-find #"^\d{9}$" pid)))))

(defn valid-passport? [passport-str]
  (let [required-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]
        passport        (parse-passport passport-str)
        passport-fields (set (keys passport))]
    (and
      (every? passport-fields required-fields)
      (valid-byr passport)
      (valid-iyr passport)
      (valid-eyr passport)
      (valid-hgt passport)
      (valid-hcl passport)
      (valid-ecl passport)
      (valid-pid passport))))

(defn count-valid-passports [batch-file-str]
  (let [passports (clojure.string/split batch-file-str #"\n\n")]
    (count (filter valid-passport? passports))))

(defn -main
  [& args]
  (println "Advent of Code 2020"))
