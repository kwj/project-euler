(ns project-euler.sol.p0015
  (:require [project-euler.lib.math :as my-math]))

(defn solve
  ([]
   (solve 20 20))
  ([x y]
   (my-math/binomial (+ x y) y)))
