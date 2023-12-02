(ns project-euler.sol.p0064
  (:require [project-euler.lib.math :as math]))

;;;;             sqrt(N) + b0        1              1
;;;;   sqrt(N) = ------------ = a0 + --,  x1 = a1 + --, ...
;;;;                  c0             x1             x2
;;;;
;;;;                   c0             c0(sqrt(N) - (b0 - a0c0))
;;;;     x1 = --------------------- = -------------------------
;;;;          sqrt(N) + (b0 - a0c0)       N - (b0 - a0c0)^2
;;;;
;;;;          sqrt(N) + (a0c0 - b0)   sqrt(N) + b1         1
;;;;        = --------------------- = ------------- = a1 + --
;;;;            N - (a0c0 - b0)^2          c1              x2
;;;;            -----------------
;;;;                   c0
;;;;    -->
;;;;      a{n} = floor( (sqrt(N)+b{n}) / c{n} )
;;;;      b{n+1} = a{n}*c{n} - b{n}
;;;;      c{n+1} = (N - b{n+1}^2) / c{n}
;;;;
;;;;      b{0} = 0, c{0} = 1, a{0} = sqrt(N)

(defn- get-cont-fraction
  [n]
  (let [isqrt-n (math/isqrt-long n)
        stop-condition (* isqrt-n 2)]
    (if (= (* isqrt-n isqrt-n) n)
      [isqrt-n, []]
      (loop [next-b isqrt-n
             c 1
             rep []]
        (let [next-c (long (quot (- n (* next-b next-b)) c))
              a (quot (+ isqrt-n next-b) next-c)]
          (if (= a stop-condition)
            [isqrt-n (conj rep a)]
            (recur (- (* a next-c) next-b) next-c (conj rep a))))))))

(defn solve
  ([]
   (solve 10000))
  ([limit]
   (->> (range 1 (inc limit))
        (map get-cont-fraction)
        (filter #(odd? (count (second %))))
        (count))))
