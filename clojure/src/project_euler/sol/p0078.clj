(ns project-euler.sol.p0078)

;;;;   p(5) = 7
;;;;   p(10) = 42
;;;;   p(50) = 204226
;;;;   p(100) = 190569292
;;;;   p(200) = 3972999029388
;;;;   p(500) = 2300165032574323995027
;;;;   p(1000) = 24061467864032622473692149727991
;;;;     ...
;;;;
;;;;   I needed to find another way instead of dynamic programming.
;;;;   Unfortunately, I gave up trying to solve it on my own at last.
;;;;
;;;;   I saw following pages.
;;;;
;;;;     https://en.wikipedia.org/wiki/Partition_(number_theory)
;;;;     https://en.wikipedia.org/wiki/Partition_function_(number_theory)
;;;;     https://en.wikipedia.org/wiki/Pentagonal_number_theorem
;;;;
;;;;     p(n) = Sigma{k ∈ Z/{0}} (-1)^(k+1) * p(n - k(3k-1)/2)
;;;;          = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-12) + p(n-15) - p(n-22) - ...
;;;;
;;;;       [p(0) = 1, p(k) = 0 when k < 0]
;;;;
;;;;   I consider only value of 'mod 1_000_000' because the problem is
;;;;   divisible by one million or not.

(defn- penta
  [i]
  (let [sign [-1 1]
        k (* (get sign (mod i 2)) (quot (inc i) 2))]
    (quot (* k (dec (* 3 k))) 2)))

(defn- right-side
  [n buff]
  (let [sign [1 1 -1 -1]]
    (loop [i 1
           result 0]
      (let [j (penta i)]
        (if (> j n)
          result
          (recur (inc i)
                 (long (+ result (* (get sign (mod (dec i) 4)) (get buff (- n j)))))))))))

(defn solve
  ([]
   (solve 1000000))
  ([modulo]
   (loop [n 1
          buff [1]]
     (let [x (mod (right-side n buff) modulo)]
       (if (zero? x)
         n
         (recur (inc n) (conj buff x)))))))
