(ns project-euler.sol.p0041
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]
   [project-euler.lib.util :as util]))

;;;; (sum_{1}_{9} n) mod 3 = 45 mod 3 = 0  --> 9-digit pandigital number is a multiple of 3.
;;;; (sum_{1}_{8} n) mod 3 = 36 mod 3 = 0  --> 8-digit pandigital number is a multiple of 3.
;;;; (sum_{1}_{7} n) mod 3 = 28 mod 3 = 1
;;;; (sum_{1}_{6} n) mod 3 = 21 mod 3 = 0  --> 6-digit pandigital number is a multiple of 3.
;;;; (sum_{1}_{5} n) mod 3 = 15 mod 3 = 0  --> 5-digit pandigital number is a multiple of 3.
;;;; (sum_{1}_{4} n) mod 3 = 10 mod 3 = 1
;;;;
;;;; We already know that 2143 is a 4-digit pandigital and is also prime.

;;; This implementation depends on the permutation lists are emitted in lexicographic ordering
;;; according to the order of the input collection.
(defn solve
  []
  (->> [7 4]
       (map #(util/permutation (range % 0 -1)))
       (apply concat)
       (map #(util/undigits (reverse %)))
       (drop-while #(or (not (math/pandigital-nz? %))
                        (not (prime/prime? %))))
       (first)))
