(ns project-euler.sol.p0056
  (:require
   [project-euler.lib.math :as my-math]
   [project-euler.lib.util :as util]))

(defn- aux
  [n upper]
  (loop [ans 0
         prod (my-math/pow n upper)
         cnt upper]
    (let [prod-digits (util/digits prod)]
      (if (or (zero? cnt)
              (< (* 9 (count prod-digits)) ans))
        ans
        (recur (max (apply + prod-digits) ans)
               (quot prod n)
               (dec cnt))))))

;;; assume that x = 10 * n
;;;   x^y = (10 * n)^y = 10^y * n^y, so sum_of_digits(x^y) = sum_of_digits(n^y)
;;;   we can skip to check multiples of ten in this problem.
(defn solve
  ([]
   (solve 100))
  ([upper]
   {:pre [(> upper 1)]}
   (let [xf (comp (remove #(zero? (mod % 10))) ; Skip when multiple of ten.
                  (map #(aux % (dec upper))))]
     (apply max (into [] xf (range (dec upper) 0 -1))))))
