(ns project-euler.sol.p0029
  (:require [project-euler.lib.math :as math]))

(defn- make-dupctr-tbl
  [upper]
  (let [max-exp (math/get-max-exp upper 2)
        dup-ctr (long-array (inc max-exp) 0)]
    (loop [xs (range 2 (inc max-exp))]
      (when-first [x xs]
        (let [dups (atom #{})]
          (loop [ys (range 1 x)]
            (when-first [y ys]
              (loop [k (quot (math/lcm x y) x)
                     is (range (max k 2) (inc (quot (* upper y) x)) k)]
                (when-first [i is]
                  (swap! dups conj i)
                  (recur k (rest is))))
              (recur (rest ys))))
          (aset dup-ctr x (count @dups)))
        (recur (rest xs))))
    (vec dup-ctr)))

(defn solve
  ([]
   (solve 100))
  ([upper]
   (let [dup-ctr (make-dupctr-tbl upper)
         base-limit (math/isqrt upper)
         skip-tbl (atom (vec (repeat (inc base-limit) false)))
         ans (atom (long (math/pow (dec upper) 2)))]
     (loop [bs (range 2 (inc base-limit))]
       (when-first [b bs]
         (when (not (get @skip-tbl b))
           (loop [es (range 2 (inc (math/get-max-exp upper b)))]
             (when-first [e es]
               (swap! ans - (nth dup-ctr e))
               (when (<= (math/pow b e) base-limit)
                 (swap! skip-tbl assoc (math/pow b e) true))
               (recur (rest es)))))
         (recur (rest bs))))
     @ans)))
