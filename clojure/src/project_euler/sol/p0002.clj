(ns project-euler.sol.p0002)

(def ^:private even-fibs
  "A lazy sequence of even fibonacci numbers."
  (lazy-cat [2 8] (map #(+ (* 4 %2) %1) even-fibs (rest even-fibs))))

(defn solve
  ([]
   (solve 4000000))
  ([upper]
   (->> (take-while #(<= % upper) even-fibs)
        (apply +))))
