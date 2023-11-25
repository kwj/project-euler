(ns project-euler.sol.p0077
  (:require [project-euler.lib.math.prime :as prime]))

(defn solve
  ([]
   (solve 5000))
  ([thr]
   (loop [n 1]
     (let [tbl (long-array (inc n) 0)]
       (aset tbl 0 1)
       (doseq [x (take n prime/prime-numbers)]
         (doseq [i (range x (inc n))]
           (aset tbl i (+ (aget tbl i) (aget tbl (- i x))))))
       (if (> (aget tbl n) thr)
         n
         (recur (inc n)))))))
