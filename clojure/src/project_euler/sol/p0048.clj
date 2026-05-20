(ns project-euler.sol.p0048
  (:require [project-euler.lib.math :as math]))

(defn solve
  ([]
   (solve 1000))
  ([upper]
   (let [mask (math/pow 10 10)
         xf (comp (filter #(pos? (mod % 10)))
                  (map #(math/powermod % % mask)))]
     (format "%010d"
             (mod (transduce xf + (range 1 (inc upper))) mask)))))
