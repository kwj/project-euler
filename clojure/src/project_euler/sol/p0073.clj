(ns project-euler.sol.p0073
  (:require [project-euler.lib.math :as math]))

(defn- make-mobius-tbl
  [limit]
  (let [p-tbl (long-array (range (inc limit)))
        mu-tbl (long-array (repeat (inc limit) 0))]
    (loop [is (range 2 (inc (math/isqrt limit)))]
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
