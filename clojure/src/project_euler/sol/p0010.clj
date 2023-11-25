(ns project-euler.sol.p0010
  (:require [project-euler.lib.math.prime :as prime]))

(defn solve
  ([]
   (solve 2000000))
  ([upper]
   {:pre [(>= upper 2)]}
   (apply + (prime/primes upper))))

