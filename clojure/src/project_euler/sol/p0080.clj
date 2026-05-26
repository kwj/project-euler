(ns project-euler.sol.p0080
  (:require
   [project-euler.lib.math :as my-math]
   [project-euler.lib.util :as util]))

(defn solve
  ([]
   (solve 100 100))
  ([upper n-digit]
   (let [pow10 (my-math/pow 10 (* (dec n-digit) 2))
         xf (comp (remove #(my-math/square? %))
                  (map #(my-math/isqrt (* % pow10))) ; Use math/isqrt due to bigint value.
                  (map #(util/digits %))
                  (map #(take n-digit (reverse %)))
                  (map #(apply + %)))]
     (transduce xf + (range 1 (inc upper))))))
