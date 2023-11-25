(ns project-euler.sol.p0060
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]))

(defn- get-candidate-primes
  [p current-min-sum]
  (cons 3 (->> (rest (rest (rest prime/prime-numbers)))
               (take-while #(and (< % p) (< (+ p %) current-min-sum)))
               (filter #(= (mod % 3) (mod p 3))))))

(defn- get-pairable-primes
  "Return a prime number sequence which elements can be concatenated with `x`
  from prime number sequence `asc-ps`. The return sequence is in descending order.
  Note: The sequence `asc-ps` must be in ascending order."
  [x asc-ps]
  (letfn [(prime-pair? [a upper-a b upper-b]
            (and (prime/prime? (+ (* a upper-b) b))
                 (prime/prime? (+ (* b upper-a) a))))]
    (let [upper-x (long (math/pow 10 (math/num-of-digits x)))]
      (loop [ps asc-ps
             upper-p 10
             result '()]
        (if (seq ps)
          (let [p (first ps)]
            (cond
              (> p upper-p) (recur ps (* upper-p 10) result)
              (prime-pair? x upper-x p upper-p) (recur (rest ps) upper-p (conj result p))
              :else (recur (rest ps) upper-p result)))
          result)))))

(defn- find-cliques
  "Return groups of concatenable primes from `desc-ps` sequence. Size of each group is `size`.
  Note: The sequence `desc-ps` must be in descending order because I check primes using the `tbl` table."
  [desc-ps size tbl]
  (let [result (transient [])]
    (letfn [(aux [group ps depth]
              (if (zero? depth)
                (conj! result group)
                (loop [ps ps]
                  (when (>= (count ps) depth)
                    (when (or (zero? (count group))
                              (every? #(contains? (get tbl %) (first ps)) group))
                      (aux (conj group (first ps)) (rest ps) (dec depth)))
                    (recur (rest ps))))))]
      (aux '() desc-ps size))
    (persistent! result)))

(defn solve
  ([]
   (solve 5))
  ([size]
   {:pre [(> size 1)]}
   (let [group-size (dec size)]
     (loop [limit Long/MAX_VALUE
            tbl {}
            [p & ps] (rest (rest (rest prime/prime-numbers)))]
       (if (> p limit)
         limit
         (let [nbrs (get-pairable-primes p (get-candidate-primes p limit))]
           (if (< (count nbrs) group-size)
             (recur limit (assoc tbl p (set nbrs)) ps)
             (let [cands (find-cliques nbrs group-size tbl)]
               (if (zero? (count cands))
                 (recur limit
                        (assoc tbl p (set nbrs))
                        ps)
                 (recur (long (apply min limit (map #(apply + p %) cands)))
                        (assoc tbl p (set nbrs))
                        ps))))))))))
