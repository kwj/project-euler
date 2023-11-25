(ns project-euler.sol.p0094)

(defn solve
  ([]
   (solve 1000000000))
  ([limit]
   (loop [a (+ (* 2 2) (* 3 1))
          b (+ 2 (* 2 1))
          acc 0]
     (let [p (case (long (mod a 3))
               2 (- (* 2 a) 2)
               1 (+ (* 2 a) 2)
               (assert false "logical error"))]
       (if (> p limit)
         acc
         (recur (+ (* 2 a) (* 3 b))
                (+ a (* 2 b))
                (long (+ acc p))))))))
