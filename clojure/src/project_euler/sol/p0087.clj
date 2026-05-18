(ns project-euler.sol.p0087
  (:require
   [clojure.math :refer [floor pow]]
   [project-euler.lib.math.prime :as prime]))

(defn solve
  ([]
   (solve 50000000))
  ([limit]
   ;; Use java.util.HashSet due to improving performance
   (let [ps (prime/primes (int (floor (pow limit (/ 1 2)))))
         z4s (sequence (comp (map #(pow % 4)) (take-while #(< % limit))) ps)
         y3s (sequence (comp (map #(pow % 3)) (take-while #(< % limit))) ps)
         x2s (map #(pow % 2) ps)
         sets (java.util.HashSet.)]
     (doseq [sums (for [z4 z4s
                        y3 y3s
                        :let [tmp (+ z4 y3)]
                        :while (< tmp limit)
                        x2 x2s
                        :while (< x2 (- limit tmp))]
                    (+ tmp x2))]
       (.add sets sums))
     (.size sets))))
