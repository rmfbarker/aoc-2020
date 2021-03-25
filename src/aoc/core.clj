(ns aoc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            )
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


;; Day 5

(defn bsp-to-binary [bsp]
  (apply str
         (map (fn [c] (if (or (= \F c)
                              (= \L c))
                        0 1))
              bsp)))

(defn bsp-to-seat-id [bsp]
  (Integer/parseInt
    (bsp-to-binary bsp) 2))

(defn your-seat []
  (first
    (drop-while
      (fn [[x y]] (= (inc x) y))
      (partition-all 2 1
                     (sort
                       (map bsp-to-seat-id
                            (read-input "input-day5")))))))

;; Day 6

(defn count-group [g]
  (count (set (apply str (clojure.string/split g #"\s+")))))

(def answer-6a
  (let [groups (clojure.string/split (slurp (io/resource "input-day6")) #"\n\n")]
    (reduce + (map count-group groups))))

(def groups (clojure.string/split (slurp (io/resource "input-day6")) #"\n\n"))

(def answer-6b
  (reduce +
          (map (fn [group]
                 (let [ppl-count (count (clojure.string/split-lines group))]
                   (count (filter #(= ppl-count %)
                                  (map count
                                       (vals
                                         (group-by identity
                                                   (remove #(= \newline %) group))))))))
               groups)))


;; Day 7
(defn parse-contents [bag-line]
  (->>
    bag-line
    (re-seq #"(\d) ([a-z]+ [a-z]+) bags?")
    (map (fn [[_ n colour]] [colour (Integer/parseInt n)]))
    (into {})))

(defn parse-outer [bag-line]
  (second (re-find #"^([a-z]+ [a-z]+) bags" bag-line)))

(defn parse-bags [bag-lines]
  (into {}
        (map (fn [l]
               [(parse-outer l) (parse-contents l)])
             bag-lines)))

(def bags (parse-bags (read-input "input-day7")))

(defn contains-shiny-gold-bags [bag]
  (let [inner-bags (keys (get bags bag))]
    (or
     (contains? (set inner-bags) "shiny gold")
     (some identity (map contains-shiny-gold-bags inner-bags)))))

(defn possible-outer-bags []
  (count (filter identity (map contains-shiny-gold-bags (keys bags)))))

(defn get-bag-count [bags type]
  (let [contents (get bags type)]
    (reduce
      (fn [total [type count]]
        (+ total count (* count (get-bag-count bags type))))
      0
      contents)))

(comment

  (get-bag-count bags "shiny gold")                         ;;=> 82930
  )


;; Day 8

(defn parse-instruction [cmd]
  (let [[operation argument] (str/split cmd #" ")
        argument (Integer/parseInt argument)]
    [operation argument]))

;; jmp (to nop) or nop (to jmp)
(defn swap-instruction [instructions pointer]
  (let [cmd (get instructions pointer)
        [operation argument] cmd]
    (cond
      (= operation "jmp") ["nop" argument]
      (= operation "nop") ["jmp" argument]
      :else [operation argument])))

(defn accumulate [instructions]

  (loop [pointer              0
         accumulator          0
         visited-instructions #{}]

    (cond

      (= (dec (count instructions)) pointer) [true accumulator]

      (contains? visited-instructions pointer) [false accumulator]

      :else (let [[operation argument] (get instructions pointer)]
              (let [[next-pointer accumulator] (condp = operation
                                                 "acc" [(inc pointer) (+ accumulator argument)]
                                                 "jmp" [(+ pointer argument) accumulator]
                                                 "nop" [(inc pointer) accumulator])]

                (recur next-pointer
                       accumulator
                       (conj visited-instructions pointer)))))))

(defn fix-program []
  (let [cmds (mapv parse-instruction (read-input "input-day8"))]
    (first
      (filter
        identity
        (for [i (range (count cmds))
              :let [new-cmd      (swap-instruction cmds i)
                    instructions (assoc cmds i new-cmd)
                    [success accumulator] (accumulate instructions)]]
          (when success accumulator))))))


;; Day 9

(def puzzle-input [35
                   20
                   15
                   25
                   47
                   40
                   62
                   55
                   65
                   95
                   102
                   117
                   150
                   182
                   127
                   219
                   299
                   277
                   309
                   576])

(def xmas (partition 5 1 puzzle-input))

(defn all-sums [ns]
  (for [x (range (count ns))
        y (range (inc x) (count ns))]
    (+ (nth ns x) (nth ns y))))

(defn is-valid [preamble-n puzzle-input]
  (let [xmas (partition preamble-n 1 puzzle-input)]
    (map
      (fn [n ns]
        (when-not (contains? (set (all-sums ns)) n)
          n))
      (drop preamble-n puzzle-input)
      xmas)))

(def day9-part1
  (let [puzzle-input (map #(Long/parseLong %) (read-input "input-day9"))]
    (first (filter identity (is-valid 25 puzzle-input)))))

(defn day9-part2 [n-stream target]

  (let [l                (count n-stream)
        partition-bounds (for [x (range l)
                               y (range (inc x) l)]
                           [x y])
        [_ lwr uppr] (first
                       (filter
                         #(= target (first %))
                         (map
                           (fn [[lwr uppr]]
                             [(reduce + (subvec n-stream lwr uppr)) lwr uppr])
                           partition-bounds)))
        contiguous-ns    (subvec n-stream lwr uppr)]
    (+ (apply min contiguous-ns)
       (apply max contiguous-ns))))

(day9-part2 [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576]
            127)                                            ;;=> 62

(day9-part2 (mapv #(Long/parseLong %) (read-input "input-day9"))
            1930745883)                                     ;;=> 268878261


;; Day 10

(defn joltage-steps [input]
  (let [device-rating (+ 3 (apply max input))
        joltages      (sort (conj input device-rating 0))]
    (map
      (fn [[k v]]
        [k (count v)])
      (group-by identity
                (map (fn [[low high]] (- high low))
                     (partition 2 1 joltages))))))

;; Examples

(joltage-steps [16 10 15 5 1 11 7 19 6 12 4])               ;;=> [[1 7] [3 5]]
(joltage-steps [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(def day10-part1
  (let [input (map #(Integer/parseInt %) (read-input "input-day10"))
        steps (joltage-steps input)]
    (reduce * (map second steps))))                         ;;=> 1920

;; => Part 2

(defn valid-combinations [joltages]
  (let [joltages    (vec (sort joltages))
        max-joltage (apply max joltages)
        built-in    (+ 3 max-joltage)
        joltages    (conj joltages built-in)
        paths-to    (reduce
                      (fn [paths j]
                        (let [valid-adapters (map #(- j %) [1 2 3])
                              ways-here      (reduce + (map #(get paths % 0) valid-adapters))]
                          (assoc paths j ways-here)))
                      {0 1}
                      joltages)]
    (get paths-to built-in)))

(defn day10-part2 []
  (let [joltages (map #(Integer/parseInt %) (read-input "input-day10"))]
    (valid-combinations joltages)))                         ;=> 1511207993344

; Day 11

(def plan (str/split-lines "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"))

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(defn step-cell [cell plan]
  (let [seat-status (get-in plan cell)
        adj         (frequencies (map #(get-in plan %)
                                      (neighbours cell)))]
    (cond (and (= \L seat-status)
               (= 0 (get adj \# 0))) \#

          (and (= \# seat-status)
               (<= 4 (get adj \# 0))) \L

          :else seat-status)))

(defn step-plan [plan]
  (let [row-l (count (first plan))]
    (mapv str/join
          (partition row-l
                     (str/join (for [row (range (count plan))
                                     col (range row-l)]
                                 (step-cell [row col] plan)))))))

(defn stable-plan [plan]
  (first (first (drop-while #(apply not= %) (partition 2 1 (iterate step-plan plan))))))

(defn seat-count [plan]
  (get (frequencies (str/join (stable-plan plan))) \#))