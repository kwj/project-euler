(ns project-euler.sol.p0091
  (:require [project-euler.lib.math :as math]))

(defn- case-1
  [x-size y-size]
  (* x-size y-size 3))

(defn- case-2
  [x-size y-size]
  (->> (for [x (range 1 (inc x-size))
             y (range 1 (inc y-size))]
         (min (quot (* y (math/gcd x y)) x)
              (quot (* (- x-size x) (math/gcd x y)) y)))
       (apply +)
       (* 2)))

(defn solve
  ([]
   (solve 50 50))
  ([x-size y-size]
   (+ (case-1 x-size y-size)
      (case-2 x-size y-size))))
