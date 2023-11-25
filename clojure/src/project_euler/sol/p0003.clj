(ns project-euler.sol.p0003
  (:require [project-euler.lib.math.prime :as prime]))

(defn solve
  ([]
   (solve 600851475143))
  ([n]
   (->> (prime/factorize n)
        (last)
        (first))))

