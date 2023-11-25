(ns project-euler.sol.p0088)

(defn solve
  ([]
   (solve 12000))
  ([limit]
   (let [tbl (long-array (repeat (inc limit) (* limit 2)))]
     (letfn [(aux [p s length num]
               (let [k (+ (- p s) length)]
                 (when (<= k limit)
                   (when (< p (aget tbl k))
                     (aset tbl k (long p)))
                   (doseq [x (range num (inc (quot (* limit 2) p)))]
                     (aux (* p x) (+ s x) (inc length) x)))))]
       (aux 1 0 0 2))
     (->> (seq tbl)
          (drop 2)
          (sort)
          (dedupe)
          (apply +)))))
