(ns project-euler.sol.p0007
  (:require [project-euler.lib.math.prime :as prime]))

(defn solve
  ([]
   (solve 10001))
  ([idx]
   {:pre [(pos? idx)]}
   (nth prime/prime-numbers (dec idx))))

