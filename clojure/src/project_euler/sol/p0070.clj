(ns project-euler.sol.p0070
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]
   [project-euler.lib.util :as util]))

;;;;   The answer must be a composite number because prime 'n' is not a permutation of phi(n) = n - 1.
;;;;
;;;;   n = p1^k1 * p2^k2 * p3^k3 * ... * p{r}^k{n}
;;;;   -->
;;;;     phi(N) = N * (1-1/p1) * (1-1/p2) * (1-1/p3) * ... * (1-1/p{n})
;;;;       <-->
;;;;     N/phi(N) = (p1/(p1-1)) * (p2/(p2-1)) * (p3/(p3-1)) * ... * (p{n}/(p{n}-1))
;;;;
;;;;   From the problem statement, 87109/phi(87109) = 87109 / 79180 = 1.1001.
;;;;   11/10 = 1.1 and 7/6 = 1.666..., so 11 <= prime numbers <= 9_999_999 / 11 = 909090.8181...
;;;;
;;;;   The answer N has the following form (p_i are prime numbers)
;;;;
;;;;     N = p1^k1 * p2^k2 * ... * pn^kn  (N < 10^7, n > 1, 11 <= p1 < p2 < ... < pn, k1>2 when n=1)

(def ^:private limit (dec (long (math/pow 10 7))))

(defn- prod
  [pf-lst]
  (reduce (fn [acc [b e]] (* acc (long (math/pow b e)))) 1 pf-lst))

(defn- phi
  [pf-lst]
  (reduce (fn [acc [b e]] (* acc (long (math/pow b (dec e))) (dec b))) 1 pf-lst))

(defn- get-phi-ratio
  [pf-lst]
  (/ (prod pf-lst) (phi pf-lst)))

(defn- make-pf-seq-aux
  [pf-lst]
  (let [[b e] (first pf-lst)
        tmp (/ limit (prod pf-lst))]
    (if (< tmp b)
      pf-lst
      (let [next-small-p (prime/prev-prime (inc tmp))]
        (if (> next-small-p b)
          (cons [next-small-p 1] pf-lst)
          (cons [b (inc e)] (rest pf-lst)))))))

(defn- make-pf-seq
  [pf-lst]
  (if (and (= (count pf-lst) 1) (= (get (first pf-lst) 1) 1))
    '()
    (cons (reverse pf-lst)
          (lazy-seq
           (let [[b e] (first pf-lst)]
             (if (> e 1)
               (make-pf-seq (cons [b (dec e)] (rest pf-lst)))
               (let [prev-p (prime/prev-prime b)
                     b2 (first (first (rest pf-lst)))
                     e2 (second (first (rest pf-lst)))]
                 (if (= prev-p b2)
                   (make-pf-seq (make-pf-seq-aux (cons [b2 (inc e2)] (rest (rest pf-lst)))))
                   (make-pf-seq (make-pf-seq-aux (cons [prev-p 1] (rest pf-lst))))))))))))

(defn- get-pf-seq
  [p1 p2]
  {:pre [(<= p1 p2)]}
  (if (= p1 p2)
    (make-pf-seq (list [p1 2]))
    (make-pf-seq (list [p2 1] [p1 1]))))

(defn- same-perm?
  [x y]
  (= (sort (util/digits x)) (sort (util/digits y))))

(defn solve
  ([]
   (solve 10000000))
  ([upper]
   {:pre [(> upper 87109)]}
   (alter-var-root #'limit (constantly (dec upper)))
   (let [pq (java.util.PriorityQueue. (reify java.util.Comparator
                                        (compare [_this x y]
                                          (cond (> (get x 0) (get y 0)) 1
                                                (< (get x 0) (get y 0)) -1
                                                :else 0))))]
     (.add pq [(/ 87109 79180) 87109])
     (loop [tpls (->> (prime/primes 11 (math/isqrt-long limit))
                      (reverse)
                      (map #(vector % (prime/prev-prime (inc (quot limit %))))))]
       (when-first [[p1 p2] tpls]
         (when (<= (get-phi-ratio (list [p1 1])) (get (.peek pq) 0))
           (loop [pf-seq (get-pf-seq p1 p2)]
             (when-first [pf-lst pf-seq]
               (when (<= (get-phi-ratio (take 2 pf-lst)) (get (.peek pq) 0))
                 (when (same-perm? (prod pf-lst) (phi pf-lst))
                   (let [ratio (get-phi-ratio pf-lst)]
                     (when (< ratio (get (.peek pq) 0))
                       (.add pq [ratio (prod pf-lst)]))))
                 (recur (rest pf-seq)))))
           (recur (rest tpls)))))
     (get (.peek pq) 1))))
