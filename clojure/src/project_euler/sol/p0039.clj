(ns project-euler.sol.p0039
  (:require [project-euler.lib.math :as math]))

;;; Primitive Pythagorean triples (variant type)
;;;   https://en.wikipedia.org/wiki/Pythagorean_triple#A_variant
;;;
;;; `m` > `n` > 0.
;;; `m` and `n` are both odd and coprime.
;;;    hypotenuse: (m^2 + n^2) / 2
;;;    catheti: mn, (m^2 - n^2) / 2
;;;    perimeter: mn + (m^2 - n^2) / 2 + (m^2 + n^2) / 2 = m(m + n)
(defn solve
  ([]
   (solve 1000))
  ([limit]
   ;; the smallest right triangle with integral lengths is the 3-4-5 right triangle.
   {:pre [(>= limit 12)]}
   (->> (for [m (range 3 (inc (math/isqrt-long limit)) 2)
              n (range 1 m 2)
              :let [p (* m (+ m n))]
              :when (and (= (math/gcd m n) 1) (<= p limit))]
          (map #(* p %) (range 1 (inc (quot limit p)))))
        (flatten)
        (frequencies)
        (seq)
        (apply max-key second) ; Note: We assume that there is only one answer.
        (first))))
