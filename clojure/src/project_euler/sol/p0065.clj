(ns project-euler.sol.p0065
  (:require [project-euler.lib.util :as util]))

;;;;   e = [2; 1, 2, 1, 1, 4, 1, 1, 6, ..., 1, 1, 2k, ...]
;;;;       [a{0}; a{1}, a{2}, ...]
;;;;
;;;;     i  a{i-1}  n(numerator)  d(denominator)
;;;;    ----------------------------------------
;;;;     1   2         2             1
;;;;     2   1         3             1
;;;;     3   2         8             3
;;;;     4   1        11             4
;;;;     5   1        19             7
;;;;     6   4        87            32
;;;;     7   1       106            39
;;;;     8   1       193            71
;;;;     9   6      1264           465
;;;;    10   1      1457           536
;;;;              ...
;;;;     i c(i)     n(i)          d(i)
;;;;
;;;;     when i > 2:
;;;;       n(i) = n(i-1)*c(i) + n(i-2), n(2) = 3, n(1) = 2
;;;;       d(i) = d(i-1)*c(i) + d(i-2), d(2) = 1, d(1) = 1
;;;;
;;;;       c(i) = | 1    (i mod 3 <> 0)
;;;;              | 2i/3 (i mod 3 = 0)

(defn- c
  [n]
  (if (zero? (mod n 3))
    (quot (*' 2 n) 3)
    1))

(def ^:private numerator-seq
  (letfn [(aux [x y idx]
            (let [next-x (+' (*' x (c idx)) y)]
              (cons next-x
                    (lazy-seq (aux next-x x (inc idx))))))]
    (concat '(2 3) (aux 3 2 3))))

(defn solve
  ([]
   (solve 100))
  ([stop]
   {:pre [(pos? stop)]}
   (->> (nth numerator-seq (dec stop))
        (util/digits)
        (apply +))))
