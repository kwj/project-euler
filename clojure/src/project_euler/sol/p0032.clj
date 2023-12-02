(ns project-euler.sol.p0032
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

;;;; m * n = mn (multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital)
;;;;
;;;; numbers of digits of multiplicand/multiplier must be 4 or less.
;;;; - if number of digits of multiplicand is 4, number of digits of multiplier is 1.
;;;; - if number of digits of multiplicand is 3, number of digits of multiplier is 2.
;;;;
;;;; multiplicand/multiplier/product : 4-digits/1-digit/4-digits or 3-digits/2-digits/4-digits

(defn solve
  []
  (letfn [(aux [a b]
            (util/undigits (concat (util/digits (* a b))
                                   (util/digits b)
                                   (util/digits a))))]
    (->> (concat (for [a (range 1001 10000)
                       b (range 2 10)
                       :when (and (not= (mod a 10) 0)
                                  (< (* a b) 10000)
                                  (math/pandigital-nz? (aux a b)))]
                   (* a b))
                 (for [a (range 101 1000)
                       b (range 11 100)
                       :when (and (not= (mod a 10) 0)
                                  (not= (mod b 10) 0)
                                  (< (* a b) 10000)
                                  (math/pandigital-nz? (aux a b)))]
                   (* a b)))
         (sort)
         (dedupe)
         (apply +))))

