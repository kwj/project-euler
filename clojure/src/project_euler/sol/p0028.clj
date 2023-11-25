(ns project-euler.sol.p0028)

(defn solve
  ([]
   (solve 1001))
  ([len]
   {:pre [(> len 1) (odd? len)]}
   (->> (range 1 (inc (quot (dec len) 2)))
        (map #(+ (* 16 % %) (* 4 %) 4))
        (apply + 1))))
