(ns aoc.core
  (:gen-class))

(defn calc-day-1 [input]
  (first
    (loop [input (sort input)]
      (for [x input
            y input
            :let [sum (+ x y)]
            :when (= 2020 sum)]
        (* x y))))
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
