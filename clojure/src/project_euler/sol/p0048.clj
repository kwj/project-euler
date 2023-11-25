(ns project-euler.sol.p0048
  (:require [project-euler.lib.math :as math]))

(defn solve
  ([]
   (solve 1000))
  ([upper]
   (let [mask (long (math/pow 10 10))]
     (format "%010d"
             (mod (->> (range 1 (inc upper))
                       (filter #(pos? (mod % 10)))
                       (map #(math/powermod % % mask))
                       (apply +))
                  mask)))))
