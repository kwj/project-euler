(ns project-euler.sol.p0001)

(defn- sum-of-multiples
  "Return the sum of multiples of `n` that each multiple is less than `upper`."
  [n upper]
  (letfn [(aux [x] (quot (* (inc x) x) 2))]
    (* (aux (quot (dec upper) n)) n)))

(defn solve
  ([]
   (solve 1000))
  ([upper]
   (- (+ (sum-of-multiples 3 upper)
         (sum-of-multiples 5 upper))
      (sum-of-multiples 15 upper))))

