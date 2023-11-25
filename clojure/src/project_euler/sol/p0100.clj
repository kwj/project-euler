(ns project-euler.sol.p0100)

(def ^:private pe-seq
  (letfn [(aux [x y]
            (lazy-seq (cons [x y]
                            (aux (+ (* 3 x) (* 4 y)) (+ (* 2 x) (* 3 y))))))]
    (aux 1 1)))

(defn solve
  ([]
   (solve 1000000000000))
  ([limit]
   {:pre [(pos? limit)]}
   (let [[_ y] (-> (drop-while (fn [[x _]] (<= x limit)) pe-seq)
                   (first))]
     (quot (inc y) 2))))
