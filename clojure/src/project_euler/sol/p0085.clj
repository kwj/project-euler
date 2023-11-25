(ns project-euler.sol.p0085
  (:require [project-euler.lib.math :as math]))

(defn- lhs
  [m n]
  (* (* m (inc m)) (* n (inc n))))

(defn- get-diff
  [m target]
  (let [n (first (drop-while #(< (lhs m %) target)
                             (iterate inc (dec (math/isqrt (quot target (* m (inc m))))))))]
    (if (< m n)
      (let [d1 (abs (- target (lhs m (dec n))))
            d2 (abs (- target (lhs m n)))]
        (if (< d1 d2)
          [d1 (dec n)]
          [d2 n]))
      nil)))

(defn solve
  ([]
   (solve 2000000))
  ([target]
   (let [new-target (* target 4)]
     (loop [m 1
            min-diff Long/MAX_VALUE
            ans 0]
       (if-let [tpl (get-diff m new-target)]
         (if (< (first tpl) min-diff)
           (recur (inc m) (long (first tpl)) (long (* m (second tpl))))
           (recur (inc m) min-diff ans))
         ans)))))
