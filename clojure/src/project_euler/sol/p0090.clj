(ns project-euler.sol.p0090
  (:require [project-euler.lib.util :as util]))

(def ^:private squares
  "Square numbers. Note: 9 is converted to 6."
  [[[0 1] [1 0]] ; 1^2
   [[0 4] [4 0]] ; 2^2
   [[0 6] [6 0]] ; 3^2
   [[1 6] [6 1]] ; 4^2
   [[2 5] [5 2]] ; 5^2
   [[3 6] [6 3]] ; 6^2
   [[4 6] [6 4]] ; 7^2, 8^2
   [[8 1] [1 8]]]) ; 9^2

(defn- check-pair
  "Check if the specified two dice can make the pair of numbers."
  [two-dice pair]
  (and (some #(= % (nth pair 0)) (first two-dice))
       (some #(= % (nth pair 1)) (second two-dice))))

(defn- check-square
  "Check if the specified two dice can make all square numbers."
  [two-dice]
  (every? #(some (fn [pair] (check-pair two-dice pair)) %) squares))

(defn solve
  []
  (->> (util/combination 6 [0 1 2 3 4 5 6 7 8 6])
       (util/combination-with-repetition 2)
       (filter #(check-square %))
       (count)))
