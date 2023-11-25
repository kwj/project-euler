(ns project-euler.sol.p0056
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn- aux
  [n upper]
  (loop [ans 0
         prod (math/pow n upper)
         cnt upper]
    (let [prod-digits (util/digits prod)]
      (if (or (zero? cnt)
              (< (* 9 (count prod-digits)) ans))
        ans
        (recur (long (max (apply + prod-digits) ans))
               (quot prod n)
               (dec cnt))))))

(defn solve
  ([]
   (solve 100))
  ([upper]
   {:pre [(> upper 1)]}
   (->> (range upper 1 -1)
        (filter #(pos? (mod % 10)))
        (map #(aux % upper))
        (apply max))))
