(ns project-euler.sol.p0006
  (:require [project-euler.lib.math :as math]))

(defn solve
  ([]
   (solve 100))
  ([upper]
   {:pre [(pos? upper)]}
   (letfn [(square-of-sum [n] (long (math/pow (quot (* n (inc n)) 2) 2)))
           (sum-of-squares [n] (quot (* n (inc n) (inc (* n 2))) 6))]
     (abs (- (square-of-sum upper) (sum-of-squares upper))))))
