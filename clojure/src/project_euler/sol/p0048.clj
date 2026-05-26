(ns project-euler.sol.p0048
  (:require [project-euler.lib.math :as my-math]))

(defn solve
  ([]
   (solve 1000))
  ([upper]
   (let [mask (my-math/pow 10 10)
         xf (comp (remove #(zero? (mod % 10)))
                  (map #(my-math/powermod % % mask)))]
     (format "%010d"
             (mod (transduce xf + (range 1 (inc upper))) mask)))))
