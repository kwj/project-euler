(ns project-euler.sol.p0058
  (:require [project-euler.lib.math.prime :as prime]))

(defn solve
  []
  (letfn [(prime-wrapper [n] (if (prime/prime? n) 1 0))]
    (loop [n 1
           nprimes 0
           acc 1]
      (let [next-nprimes (long (+ nprimes
                                  (prime-wrapper (+ acc (* 2 n)))
                                  (prime-wrapper (+ acc (* 4 n)))
                                  (prime-wrapper (+ acc (* 6 n)))))]
        (if (< (/ next-nprimes (inc (* 4 n))) 0.1)
          (inc (* 2 n))
          (recur (inc n) next-nprimes (+ acc (* 8 n))))))))
