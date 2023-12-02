(ns project-euler.sol.p0086
  (:require [project-euler.lib.math :as math]))

;;;;   1 <= a, b, c <= M
;;;;
;;;;   we can ignore rotations. there is only one case to consider.
;;;;     1 <= a <= b <= c <= M
;;;;      --> 2 <= a + b <= 2c
;;;;
;;;;       +--------F
;;;;       |        |      * sqrt(c^2 + (a+b)^2) must be an integer
;;;;       |--------|
;;;;       |        | a+b >= 2
;;;;       |        |
;;;;       S--------+
;;;;            c
;;;;
;;;;   when a+b <= c <= M
;;;;     write a+b = x
;;;;       (a, b) = (1, x-1), (2, x-2), ..., (x-1, 1)
;;;;     however, because a<=b
;;;;       num of (a,b) = floor(x/2) = floor((a+b)/2)
;;;;
;;;;   when a+b > c
;;;;       num of (a,b) = floor((a+b)/2) - ((a+b-1) - c)
;;;;
;;;;       example: c=10, a+b=15
;;;;         (a,b) = (1,14), ..., (5,10), (6,9), (7,8), ..., (14,1)
;;;;                              ####################
;;;;                 ^^^^^^^^^^^ = (a+b-1) - c
;;;;                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)
;;;;
;;;;       example: c=10, a+b=16
;;;;         (a,b) = (1,15), ..., (6,10), (7,9), (8,8), ..., (15,1)
;;;;                              ####################
;;;;                 ^^^^^^^^^^^ = (a+b-1) - c
;;;;                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)

(defn- aux
  ^long [^long c]
  (loop [ab (* c 2)
         acc 0]
    (if (> ab 1)
      (let [tmp (+ (* c c) (* ab ab))
            tmp-isqrt (math/isqrt-long tmp)]
        (if (= (* tmp-isqrt tmp-isqrt) tmp)
          (cond
            (<= ab c) (recur (dec ab) (+ acc (quot ab 2)))
            :else (recur (dec ab) (+ acc (- (quot ab 2) (- ab 1 c)))))
          (recur (dec ab) acc)))
      acc)))

(defn solve
  ([]
   (solve 1000000))
  ([thr]
   (loop [c 3
          cnt 0]
     (if (> cnt thr)
       (dec c)
       (recur (inc c) (+ cnt (aux c)))))))
