(ns project-euler.sol.p0060
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]))

(def ^:private primes-rem1
  (concat [3] (filter #(= (mod % 3) 1) (rest (rest prime/prime-numbers)))))

(def ^:private primes-rem2
  (concat [3] (filter #(= (mod % 3) 2) (rest (rest prime/prime-numbers)))))

(defn- get-candidate-primes
  [p current-min-sum]
  (let [ps (cond
             (= (mod p 3) 1) primes-rem1
             (= (mod p 3) 2) primes-rem2)]
    (take-while #(and (< % p) (< (+ p %) current-min-sum)) ps)))

(defn- get-pairable-primes
  "Return a prime number sequence which elements can be concatenated with `x`
  from prime number sequence `asc-ps`. The return sequence is in descending order.
  Note: The sequence `asc-ps` must be in ascending order."
  [x asc-ps]
  (letfn [(prime-pair? [a upper-a b upper-b]
            (and (prime/fermat-prime? (+ (* a upper-b) b))
                 (prime/fermat-prime? (+ (* b upper-a) a))))]
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

(defn- clique?
  [lst]
  (letfn [(prime-pair? [a b]
            (let [x (+ (* b (math/pow 10 (math/num-of-digits a))) a)
                  y (+ (* a (math/pow 10 (math/num-of-digits b))) b)]
              (and (prime/prime? x) (prime/prime? y))))]
    (loop [lst lst]
      (if-let [[x & xs] (seq lst)]
        (cond
          (every? #(prime-pair? x %) xs) (recur xs)
          :else false)
        true))))

(defn- find-cliques
  "Return groups of concatenable primes from `desc-ps` sequence. Size of each group is `size`.
  Note: The sequence `desc-ps` must be in descending order because I check primes using the `tbl` table."
  [p desc-ps size tbl]
  (let [result (transient [])]
    (letfn [(aux [group ps depth]
              (if (zero? depth)
                (conj! result group)
                (loop [ps ps]
                  (when (>= (count ps) depth)
                    (when (every? #(contains? (get tbl %) (first ps)) group)
                      (aux (conj group (first ps)) (rest ps) (dec depth)))
                    (recur (rest ps))))))]
      (aux '() desc-ps size))
    (filter #(clique? (conj % p)) (persistent! result))))

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
             (let [cands (find-cliques p nbrs group-size tbl)]
               (if (zero? (count cands))
                 (recur limit
                        (assoc tbl p (set nbrs))
                        ps)
                 (recur (long (apply min limit (map #(apply + p %) cands)))
                        (assoc tbl p (set nbrs))
                        ps))))))))))
