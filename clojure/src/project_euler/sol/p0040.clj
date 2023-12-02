(ns project-euler.sol.p0040
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

;;;;   0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
;;;;     ---------   -----------------   ---------------------   -----------------
;;;; len: 1 * 9       2 * 90              3 * 900                 4 * 9000      ...
;;;;      1 * 9 * 1   2 * 9 * 10          3 * 9 * 100             4 * 9 * 1000  ...
;;;;        --> block_num * 9 * base
;;;;
;;;;  block #1: 1-digit number
;;;;  block #2: 2-digits number
;;;;  block #3: 3-digits number
;;;;    ...
;;;;  block #n: n-digits number

(def ^:private block-info
  (reductions (fn [[ndigit max-ith] x] [x (+ max-ith (* 9 x (math/pow 10 ndigit)))])
              [1 9]
              (iterate inc 2)))

(defn- get-block-info
  [ith]
  (first (drop-while #(> ith (second %)) block-info)))

(defn- d
  [ith]
  {:pre [(pos? ith)]}
  (let [[ndigit max-ith] (get-block-info ith)
        n (- (math/pow 10 ndigit) 1 (quot (- max-ith ith) ndigit))
        place-idx (mod (- max-ith ith) ndigit)]
    (nth (util/digits n) place-idx)))

(defn solve
  []
  (->> (range 0 7)
       (map #(d (math/pow 10 %)))
       (apply *)))
