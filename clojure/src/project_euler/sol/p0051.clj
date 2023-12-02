(ns project-euler.sol.p0051
  (:require
   [project-euler.lib.math.prime :as prime]
   [project-euler.lib.util :as util]))

;;;; The smallest prime which, by replacing part of the number with same digit,
;;;; is part of an eight prime value family
;;;;
;;;;  -> Either 0, 1, or 2 must be used for replacing digit.
;;;;
;;;; 1) the last digit is not eligible for replacement
;;;;    It make some even numbers after replacement.
;;;;
;;;; 2) the number of digits of the prime numbers is greater than number of digits to be replaced
;;;;    the reason is since 1).
;;;;
;;;; 3) the number of digits that can be replaced is only a multiples of 3
;;;;    if 'n' is not multiples of 3, some replaced numbers will contain multiples of 3.
;;;;
;;;;    number  digits  sum  'mod 3'    [n>0]
;;;;    ---------------------------------------------
;;;;    0       n       0    0
;;;;    1       n       n    n mod 3
;;;;    2       n       2n   2n mod 3
;;;;    3       n       3n   3n mod 3 = 0
;;;;    4       n       4n   4n mod 3 = n mod 3
;;;;    5       n       5n   5n mod 3 = 2n mod 3
;;;;    6       n       6n   6n mod 3 = 0
;;;;    7       n       7n   7n mod 3 = n mod 3
;;;;    8       n       8n   8n mod 3 = 2n mod 3
;;;;    9       n       9n   9n mod 3 = 0
;;;;
;;;; 4) There are at least same three numbers other than last digit.

(defn- get-family-primes
  [p-digits start-num mask-lst]
  (let [p-vec (vec p-digits)
        nums (for [d (range start-num 10)]
               (loop [v p-vec
                      lst mask-lst]
                 (if (seq lst)
                   (recur (assoc v (first lst) d) (rest lst))
                   (util/undigits v))))]
    (filter #(prime/prime? %) nums)))

(defn- prime-family?
  [p size]
  (let [p-digits (util/digits p)]
    (loop [ns (range 0 (inc (- 10 size)))]
      (if (seq ns)
        (let [n (first ns)
              groups (for [mask (->> (util/find-all #(= % n) p-digits)
                                     (util/powerset)
                                     (filter #(and (zero? (mod (count %) 3))
                                                   (not (zero? (count %)))
                                                   (not (zero? (nth % 0))))))]
                       (get-family-primes p-digits n mask))]
          (if (some #(>= (count %) size) groups)
            true
            (recur (rest ns))))
        false))))

(defn solve
  ([]
   (solve 8))
  ([size]
   {:pre [(pos? size)]}
   (->> prime/prime-numbers
        (drop-while #(< % 1000))
        (drop-while #(not (prime-family? % size)))
        (first))))
