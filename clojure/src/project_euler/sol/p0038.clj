(ns project-euler.sol.p0038
  (:require [project-euler.lib.math :as math]))

(defn- get-pandigital-numbers
  [xs]
  (for [x xs
        :let [n (* x 100002) ; x * 10^5 + x * 2 -> x * 100002
              r (mod x 10)]
        :when (and (or (= r 2) (= r 3) (= r 6) (= r 7))
                   (math/pandigital-nz? n))]
    n))

(defn solve
  []
  (->> (range 9183 9500)
       (get-pandigital-numbers)
       (apply max 918273645)))
