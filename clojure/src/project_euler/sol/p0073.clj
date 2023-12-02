(ns project-euler.sol.p0073
  (:require [project-euler.lib.math :as math]))

;;;; Möbius inversion formula
;;;;
;;;; f(n): number of fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2
;;;;       --> sigma{i=1, ...,n}((i-1)//2 - i//3)
;;;; g(n): number of irreducible fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2, gcd(a,b)=1
;;;;
;;;;   The answer we should seek is g(12000).
;;;;
;;;; f(n) = sigma{k=1, ..., n}(g(n//k))
;;;; -->
;;;;   g(n) = sigma{k=1, ..., n}μ(k)f(n//k)      [möbius inversion formula, μ(): möbius function]
;;;;        = sigma{k=1, ..., n}μ(k)sigma{j=1, ..., n//k}((j-1)//2 - j//3)

(defn- make-mobius-tbl
  [limit]
  (let [p-tbl (long-array (range (inc limit)))
        mu-tbl (long-array (repeat (inc limit) 0))]
    (loop [is (range 2 (inc (math/isqrt-long limit)))]
      (when-first [i is]
        (when (= (aget p-tbl i) i)
          (loop [js (range (* i i) (inc limit) i)]
            (when-first [j js]
              (aset p-tbl j (long i))
              (recur (rest js))))
          (loop [js (range (* i i) (inc limit) (* i i))]
            (when-first [j js]
              (aset p-tbl j 0)
              (recur (rest js)))))
        (recur (rest is))))
    (aset mu-tbl 1 1)
    (loop [is (range 2 (inc limit))]
      (when-first [i is]
        (when (not= (aget p-tbl i) 0)
          (aset mu-tbl i (- (aget mu-tbl (quot i (aget p-tbl i))))))
        (recur (rest is))))
    (vec mu-tbl)))

(defn- f
  [x]
  (->> (range 1 (inc x))
       (map #(- (quot (- % 1) 2) (quot % 3)))
       (apply +)))

(defn- g
  [limit]
  (let [mu-tbl (make-mobius-tbl limit)]
    (->> (range 1 (inc limit))
         (map #(* (get mu-tbl %) (f (quot limit %))))
         (apply +))))

(defn solve
  ([]
   (solve 12000))
  ([limit]
   (g limit)))
