(ns project-euler.sol.p0045)

(def ^:private gpe-solutions
  (letfn [(aux [x y]
            (lazy-seq (cons [x y]
                            (aux (+ (* 2 x) (* 3 y)) (+ x (* 2 y))))))]
    (aux 1 1)))

(defn solve
  ([]
   (solve 3))
  ([ith]
   {:pre [(pos? ith)]}
   (let [[_ y] (-> (filter (fn [[x y]] (and (= (mod x 6) 5) (= (mod y 4) 3))) gpe-solutions)
                   (nth (dec ith)))
         j (quot (inc y) 4)]
     (* j (dec (* 2 j))))))
