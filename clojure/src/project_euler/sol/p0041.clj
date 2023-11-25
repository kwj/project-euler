(ns project-euler.sol.p0041
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]
   [project-euler.lib.util :as util]))

(defn solve
  []
  (->> [7 4]
       (map #(util/permutation (range % 0 -1)))
       (apply concat)
       (map #(util/undigits (reverse %)))
       (drop-while #(or (not (math/pandigital-nz? %))
                        (not (prime/prime? %))))
       (first)))
