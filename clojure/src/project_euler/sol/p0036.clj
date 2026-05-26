(ns project-euler.sol.p0036
  (:require [project-euler.lib.math :as my-math]))

(defn solve
  ([]
   (solve 1000000))
  ([limit]
   (->> (range 1 limit 2)
        (filter #(and (my-math/palindrome? % 10) (my-math/palindrome? % 2)))
        (reduce +))))
