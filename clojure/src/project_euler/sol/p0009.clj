(ns project-euler.sol.p0009
  (:require [project-euler.lib.math :as math]))

;;; a = k(m^2 - n^2), b = k * 2mn, c = k(m^2 + n^2)  [m>n>0, gcd(m,n)=1, m+n is odd]
;;;
;;; a + b + c = k * 2m(m+n) = 1000
;;; abc = k^3 * (m^4 - n^4) * 2mn

(defn solve
  ([]
   (solve 1000))
  ([perim]
   {:pre [(pos? perim)]}
   (let [half-perim (quot perim 2)
         ;; The problem statement mentions there exists exactly only
         ;; one Pythagorean triplet when the perimeter is 1000.
         [m n] (first (for [m (range 2 (inc (math/isqrt half-perim)))
                            n (range (inc (mod m 2)) (inc (quot (- 500 m) m)) 2)
                            :when (and (> m n)
                                       (= (math/gcd m n) 1)
                                       (zero? (mod half-perim m))
                                       (zero? (mod half-perim (+ m n)))
                                       (zero? (mod half-perim (* m (+ m n)))))]
                        [m n]))
         k (quot (quot half-perim m) (+ m n))]
     (long (* (math/pow k 3) (- (math/pow m 4) (math/pow n 4)) 2 m n)))))
