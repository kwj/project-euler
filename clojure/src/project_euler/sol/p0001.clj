(ns project-euler.sol.p0001)

(defn- sum-of-multiples
  "Return the sum of multiples of `n` less than `upper`."
  [n upper]
  (let [upper (dec upper)
        aux #(quot (* (+ % 1) %) 2)]
    (* (aux (quot upper n)) n)))

(defn solve
  ([]
   (solve 1000))
  ([upper]
   (- (+ (sum-of-multiples 3 upper)
         (sum-of-multiples 5 upper))
      (sum-of-multiples 15 upper))))

