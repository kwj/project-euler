(ns project-euler.sol.p0044
  (:require [project-euler.lib.math :as math]))

(defn pent
  "Return a `n`-th pentagonal number."
  [n]
  (quot (* n (dec (* 3 n))) 2))

;;; Assume that k > j and x = k - j:
;;;
;;; Pd = P{k} - P{j}
;;;    = P{x+j} - P{j}
;;;    = 3jx + P{x}
;;;
;;; j = (P{d} - P{x}) / 3x
(defn solve
  []
  (loop [d 4]
    (let [pd (pent d)
          pairs (for [x (range (- d 3) 0 -3)
                      :let [px (pent x)]
                      :when (zero? (mod (- pd px) (* 3 x)))
                      :let [j (quot (- pd px) (* 3 x))
                            k (+ x j)]
                      :when (math/pentagonal? (+ (pent j) (pent k)))]
                  [j k])]
      (if (seq pairs)
        pd
        (recur (inc d))))))
