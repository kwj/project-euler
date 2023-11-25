(ns project-euler.sol.p0015
  (:require [project-euler.lib.math :as math]))

(defn solve
  ([]
   (solve 20 20))
  ([x y]
   (math/binomial (+ x y) y)))

