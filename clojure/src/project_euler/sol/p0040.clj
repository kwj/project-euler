(ns project-euler.sol.p0040
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

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
