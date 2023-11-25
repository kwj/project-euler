(ns project-euler.sol.p0036
  (:require [project-euler.lib.math :as math]))

(defn solve
  ([]
   (solve 1000000))
  ([limit]
   (->> (range 1 limit 2)
        (filter #(and (math/palindrome? % 10) (math/palindrome? % 2)))
        (apply +))))

