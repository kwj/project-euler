(ns project-euler.sol.p0026
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]))

(defn- pp
  "Remove prime factors 2 and 5 from `n`."
  ^long [n]
  (letfn [(aux [n denom]
            (loop [n n]
              (if (zero? (mod n denom))
                (recur (quot n denom))
                n)))]
    (aux (aux n 2) 5)))

;;; This function is not strictly the correct Carmichael function
;;; because the function assumes that the argument is not a multiple of 2.
(defn- carmichael
  [n]
  (->> (prime/factorize n)
       (map (fn [[base exp]] (* (dec base) (long (math/pow base (dec exp))))))
       (reduce math/lcm)))

(defn- find-repetend-length
  ^long [d]
  (let [d (pp d)]
    (if (= d 1)
      0
      (first (for [k (prime/divisors (carmichael d))
                   :when (= (math/powermod 10 k d) 1)]
               k)))))

(defn solve
  ([]
   (solve 1000))
  ([upper]
   (loop [nums (range (dec upper) (dec (quot upper 2)) -1)
          max-length 0
          answer 0]
     (when-first [i nums]
       (if (<= i max-length)
         answer
         (let [repetend-length (find-repetend-length i)]
           (if (> repetend-length max-length)
             (recur (rest nums) repetend-length (pp i))
             (recur (rest nums) max-length answer))))))))

