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
    (slurp (io/resource filename))))

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


(map
  (fn [pwd-policy]
    (let [[count [character _] password] (str/split pwd-policy #" ")]
      [count character password]))
  (read-input "input-day2"))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
