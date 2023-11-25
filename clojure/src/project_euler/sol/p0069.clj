(ns project-euler.sol.p0069
  (:require [project-euler.lib.math.prime :as prime]))

(defn solve
  ([]
   (solve 1000000))
  ([upper]
   (->> (reductions * prime/prime-numbers)
        (take-while #(<= % upper))
        (last))))
