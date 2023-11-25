(ns project-euler.sol.p0005
  (:require [project-euler.lib.math :as math]))

(defn solve
  ([]
   (solve 20))
  ([upper]
   {:pre [(pos? upper)]}
   (reduce math/lcm (range 1 (inc upper)))))

