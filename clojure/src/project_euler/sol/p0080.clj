(ns project-euler.sol.p0080
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn solve
  ([]
   (solve 100 100))
  ([upper n-digit]
   (let [pow10 (math/pow 10 (* (dec n-digit) 2))]
     (->> (range 1 (inc upper))
          (filter #(not (math/square? %)))
          (map #(math/isqrt (* % pow10))) ; Use math/isqrt because bigint value.
          (map #(util/digits %))
          (map #(apply + %))
          (apply +)))))
