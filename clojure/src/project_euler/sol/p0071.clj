(ns project-euler.sol.p0071)

(defn solve
  ([]
   (solve 1000000))
  ([limit]
   (+ 2 (* 3 (quot (- limit 5) 7)))))
