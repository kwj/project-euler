(ns project-euler.sol.p0020
  (:require
   [project-euler.lib.math :as my-math]
   [project-euler.lib.util :as util]))

(defn solve
  ([]
   (solve 100))
  ([n]
   (apply + (util/digits (my-math/factorial n)))))
