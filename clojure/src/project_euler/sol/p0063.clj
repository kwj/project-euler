(ns project-euler.sol.p0063
  (:require [clojure.math]))

(defn solve
  []
  (->> (range 1 10)
       (map #(int (clojure.math/floor (/ 1.0 (- 1.0 (clojure.math/log10 %))))))
       (apply +)))
