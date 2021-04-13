(ns aoc.day12
  (:require [aoc.core :refer [read-input]]))

(defn make-ship []
  {:orientation :east :x 0 :y 0})

(defn move-ship [{:keys [x y] :as ship} dir steps]
  (condp = dir
    :north (update ship :y + steps)
    :east (update ship :x + steps)
    :south (update ship :y - steps)
    :west (update ship :x - steps)))

(defn move-waypoint [ship dir steps]
  (condp = dir
    :north (update-in ship [:waypoint :y] + steps)
    :east (update-in ship [:waypoint :x] + steps)
    :south (update-in ship [:waypoint :y] - steps)
    :west (update-in ship [:waypoint :x] - steps)))

(defn move-forward [{:keys [orientation] :as ship} steps]
  (move-ship ship orientation steps))

(defn parse-cmd [cmd]
  (let [dir   (subs cmd 0 1)
        steps (Integer/parseInt (subs cmd 1))]
    [dir steps]))

(defn rotate [ship cmd]
  (let [[v s] (parse-cmd cmd)
        co-ords         [:east :north :west :south]
        dirs            (cycle
                          (if (= v "L")
                            co-ords
                            (reverse co-ords)))
        turns           (/ s 90)
        new-orientation (nth (drop-while
                               #(not= (:orientation ship) %)
                               dirs)
                             turns)]
    (assoc ship :orientation new-orientation)))

(defn move [ship cmd]
  (let [[dir steps] (parse-cmd cmd)]
    (condp = dir
      "F" (move-forward ship steps)

      "N" (move-ship ship :north steps)
      "E" (move-ship ship :east steps)
      "S" (move-ship ship :south steps)
      "W" (move-ship ship :west steps)

      "L" (rotate ship cmd)
      "R" (rotate ship cmd))))

(defn move-part2 [ship cmd]
  (let [[dir steps] (parse-cmd cmd)]
    (condp = dir
      "F" (move-forward ship steps)

      "N" (move-ship ship :north steps)
      "E" (move-ship ship :east steps)
      "S" (move-ship ship :south steps)
      "W" (move-ship ship :west steps)

      "L" (rotate ship cmd)
      "R" (rotate ship cmd))))

(defn move-waypoint [waypoint cmd]

)

(defn manhattan-dist [ship]
  (let [{:keys [x y]} ship]
    (+ (Math/abs x) (Math/abs y))))

(comment
  (manhattan-dist
    (reduce
      move
      (make-ship)
      (read-input
       "input-day12")))                          ;; => 998
  )
