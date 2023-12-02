(ns project-euler.sol.p0066
  (:require [project-euler.lib.math :as math]))

;;;;   X^2 - N * Y^2 = 1
;;;;   -----------------
;;;;
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
;;;;      b{n+1} = a{n}*c{n} - b{n}
;;;;      c{n+1} = (N - b{n+1}^2) / c{n}
;;;;      a{n+1} = floor((sqrt(N)+b{n+1}) / c{n+1}) = (floor(sqrt(N)) + b{n+1}) / c{n+1}
;;;;
;;;;        a{0} = floor(sqrt(N)), b{0} = 0, c{0} = 1
;;;;
;;;;
;;;;   write A{0}, A{1}, A{2}, ...
;;;;            x{0}                          x{1}                      1         x{2}
;;;;     a{0} = ---- = A{0},  a{0} + 1/a{1} = ---- = A{1},  a{0} + ------------ = ---- = A{2},  ...
;;;;            y{0}                          y{1}                         1      y{2}
;;;;                                                               a{1} + ----
;;;;                                                                      a{2}
;;;;                                               [x{0} = a{0}, y{0} = 1]
;;;;    -->
;;;;         n=0: -> x{0} = a{0}, y{0} = 1
;;;;         n=1: -> x{1} = a{0}*a{1} + 1, y{1} = a{1}
;;;;
;;;;         n=2: -> x{2}/y{2}
;;;;                      = (a{0}*a{1}*a{2} + a{0} + a{2}) / (a{1}a{2} + 1)
;;;;
;;;;                        a{2}*(a{0}*a{1} + 1) + a{0}   a{2}*x{1} + a{0}
;;;;                      = --------------------------- = ----------------
;;;;                             a{2}*a{1} + 1            a{2}*y(1) + 1
;;;;
;;;;                        a{2}*x{1} + x{0}
;;;;                      = ----------------
;;;;                        a{2)*y{1} + y{0}
;;;;
;;;;                                     a{k}*x{k-1} + x{k-2}
;;;;      assume that A{k} = x{k}/y{k} = --------------------  [k>=2]
;;;;                                     a{k}*y{k-1} + y{k-2}
;;;;
;;;;                                  ((a{k}*a{k+1} + 1)/a{k+1})*x{k-1} + x{k-2}
;;;;         A{k+1} = x{k+1}/y{k+1} = -----------------------------------------
;;;;                                  ((a{k}*a{k+1} + 1)/a{k+1})*y{k-1} + y{k-2}
;;;;
;;;;                                  (a{k}*a{k+1} + 1)*x{k-1} + x{k-2}*a{k+1}
;;;;                                = -----------------------------------------
;;;;                                  (a{k}*a{k+1} + 1)*y{k-1} + y{k-2}*a{k+1}
;;;;
;;;;                                  a{k+1}(a{k}*x{k-1} + x{k-2}) + x{k-1}
;;;;                                = -------------------------------------
;;;;                                  a{k+1}(a{k}*y{k-1} + y{k-2}) + y{k-1}
;;;;
;;;;                                  a{k+1}*x{k} + x{k-1}
;;;;                                = --------------------
;;;;                                  a{k+1}*y{k} + y{k-1}
;;;;       -->
;;;;         x{k+1} = a{k+1} * x{k} + x{k-1}
;;;;         y{k+1} = a{k+1} * y{k} + y{k-1}
;;;;
;;;;    -->
;;;;      [a{0}; a{1], a{2}, ...]
;;;;      assume that x{-1} = 1, x{0} = a{0}, y{-1} = 0, y{0} = 1
;;;;
;;;;      [n>=1]
;;;;        x{n} = a{n} * x{n-1} + x{n-2}
;;;;        y{n} = a{n} * y{n-1} + y{n-2}

;;; From `Problem 64`
(defn- get-cont-fraction
  [n]
  (let [isqrt-n (math/isqrt-long n)
        stop-condition (* isqrt-n 2)]
    (if (= (* isqrt-n isqrt-n) n)
      [isqrt-n, []]
      (loop [b isqrt-n
             c 1
             rep []]
        (let [next-c (long (quot (- n (* b b)) c))
              a (quot (+ isqrt-n b) next-c)]
          (if (= a stop-condition)
            [isqrt-n (conj rep a)]
            (recur (- (* a next-c) b) next-c (conj rep a))))))))

(defn- get-numerator
  [a0 rep-lst]
  (first (reduce (fn [[x1 x2] a] [(+' (*' a x1) x2) x1]) [a0 1] rep-lst)))

(defn solve
  ([]
   (solve 1000))
  ([limit]
   (->> (range 1 (inc limit))
        (map #(vector % (get-cont-fraction %)))
        (filter #(not (zero? (count (second (second %))))))
        (map (fn [[i [a0 lst]]]
               (if (even? (count lst))
                 [i (get-numerator a0 (drop-last lst))]
                 [i (get-numerator a0 (drop-last (concat lst lst)))])))
        (apply max-key #(second %))
        (first))))
