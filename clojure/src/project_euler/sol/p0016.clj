(ns project-euler.sol.p0016
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn solve
  ([]
   (solve 1000))
  ([exp]
   (apply + (util/digits (math/pow 2 exp)))))

