(ns project-euler.sol.p0014)

(defn- get-collatz-length
  "Return the length of the Collatz sequence of `n`."
  [^long n]
  (loop [n n
         cnt 1]
    (cond
      (= n 1) cnt
      (even? n) (recur (quot n 2) (inc cnt))
      :else (recur (inc (* 3 n)) (inc cnt)))))

(defn solve
  ([]
   (solve 1000000))
  ([thr]
   (->> (range (quot thr 2) thr)
        (map #(vector % (get-collatz-length %)))
        (apply max-key second)
        (first))))

