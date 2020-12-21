(ns aoc.core
  (:require [clojure.java.io :as io])
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

(def input-day1 (map #(Integer/parseInt %)
      (clojure.string/split-lines
        (slurp (io/resource "input-day1")))))

(def answer-1 (calc-day-1 input-day1))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
