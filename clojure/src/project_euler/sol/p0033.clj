(ns project-euler.sol.p0033
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

;;;; case #1: (10a + c) / (10c + b) = a / b
;;;; case #2: (10a + c) / (10b + c) = a / b
;;;; case #3: (10c + a) / (10c + b) = a / b
;;;; case #4: (10c + a) / (10b + c) = a / b
;;;; // prerequisite: a < b
;;;;
;;;; case #1:
;;;;   10ab + bc = 10ac + ab
;;;;   -> 9ab = 10ac - bc
;;;;   -> 9ab - 9ac = ac - bc
;;;;   -> 9a(b - c) = c(a - b)
;;;;   -> 9a(c - b) = c(b - a)  [a < b => a < b < c]
;;;;
;;;; case #2:
;;;;   10ab + bc = 10ab + ac
;;;;   -> bc = ac
;;;;   -> b = a   ==> NG (contradiction)
;;;;
;;;; case #3:
;;;;   10bc + ab = 10ac + ab
;;;;   -> 10bc = 10ac
;;;;   -> b = a   ==> NG (contradiction)
;;;;
;;;; case #4:
;;;;   10bc + ab = 10ab + ac
;;;;   -> 10bc - ac = 9ab
;;;;   -> bc - ac = 9ab - 9bc
;;;;   -> c(b - a) = 9b(a - c)  [a < b => c < a < b]
;;;;   -> a - c = c/9 - ac/9b => 1   ==> NG (bacause c/9 < 1)

(defn solve
  []
  (let [[numerator denominator]
        (->> (util/combination 3 (range 1 10))
             (filter (fn [[a b c]] (= (* 9 a (- c b)) (* c (- b a)))))
             (reduce (fn [[n0 d0] [n1 d1 _]] [(* n0 n1) (* d0 d1)]) [1 1]))]
    (quot denominator (math/gcd numerator denominator))))

