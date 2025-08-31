(ns project-euler.sol.p0085
  (:require [project-euler.lib.math :as math]))

;;;;   nCr = n! / ((n-r)! * r!)
;;;;
;;;;       1  2       n-1  n
;;;;     +--+--+-- ... --+--+
;;;;    1|  |  |   ...   |  |
;;;;     +--+--+-- ... --+--+
;;;;    2|  |  |   ...   |  |
;;;;     +--+--+-- ... --+--+ num of horizontal lines = m + 1
;;;;    3|  |  |   ...   |  |
;;;;     +--+--+-- ... --+--+
;;;;     ....................
;;;;     +--+--+-- ... --+--+
;;;;    m|  |  |   ...   |  |
;;;;     +--+--+-- ... --+--+
;;;;       num of vertical lines = n + 1
;;;;
;;;;   (m+1)C2 * (n+1)C2 = m(m+1)/2 * n(n+1)/2 (\approx) 2_000_000
;;;;   --> m(m+1)*n(n+1) (\approx) 8_000_000

(defn- lhs
  [m n]
  (* m (inc m) n (inc n)))  ;; m(m+1) * n(n+1)

(defn- get-diff
  [m target]
  (let [n (first (drop-while #(< (lhs m %) target)
                             (iterate inc (dec (math/isqrt-long (quot target (* m (inc m))))))))]
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
