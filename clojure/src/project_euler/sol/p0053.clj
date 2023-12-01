(ns project-euler.sol.p0053)

(defn solve
  ([]
   (solve 100 1000000))
  ([n thr]
   {:pre [(pos? n) (pos? thr)]}
   (loop [n n
          x n
          c n
          r 1
          ans 0]
     (if (<= r (quot n 2))
       (if (> c thr)
         (recur (dec n) (dec x) (quot (* c (dec x)) n) r (long (- (+ ans n 1) (* r 2))))
         (recur n (dec x) (quot (* c (dec x)) (inc r)) (inc r) ans))
       ans))))
