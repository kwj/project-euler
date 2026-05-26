(ns project-euler.sol.p0075
  (:require [project-euler.lib.math :as my-math]))

;;;; Pythagorean triple
;;;;
;;;;   a = k * (m^2 - n^2), b = k * 2mn, c = k * (m^2 + n^2)
;;;;     where m > n > 0, gcd(m, n) = 1
;;;;
;;;;   perimeter L = k * (2m^2 + 2mn)
;;;;               = k * 2m(m + n)
;;;;
;;;;   2m(m + n) = L/k
;;;;     -->
;;;;   2m^2 < 2m(m + n) = L/k
;;;;     <-->
;;;;   m^2 < L/2k
;;;;
;;;;   'm' is maximized when k=1
;;;;     max(m) < sqrt(L/2)

(defn solve
  ([]
   (solve 1500000))
  ([perim]
   (let [tbl (long-array (repeat (inc perim) 0))]
     (doseq [m (range 2 (inc (my-math/isqrt-long (quot perim 2))))]
       (doseq [n (range (inc (mod m 2)) (inc m) 2)]
         (when (= (my-math/gcd m n) 1)
           (let [p (* 2 m (+ m n))]
             (when (<= p perim)
               (doseq [i (range p (inc perim) p)]
                 (aset tbl i (inc (aget tbl i)))))))))
     (count (filter #(= % 1) tbl)))))
