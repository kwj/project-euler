(ns project-euler.sol.p0078)

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
