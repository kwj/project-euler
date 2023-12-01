(ns project-euler.sol.p0055
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn- rychrel?
  [n]
  (letfn [(reverse-num [n] (-> (util/digits n) (reverse) (util/undigits)))]
    (loop [n n
           tmp (reverse-num n)
           cnt 50]
      (if (zero? cnt)
        true
        (let [next-n (+' n tmp)]
          (if (math/palindrome? next-n)
            false
            (recur next-n (reverse-num next-n) (dec cnt))))))))

(defn solve
  ([]
   (solve 10000))
  ([upper]
   {:pre [(pos? upper)]}
   (->> (range 1 upper)
        (filter rychrel?)
        (count))))
