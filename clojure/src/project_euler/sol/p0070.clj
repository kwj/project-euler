(ns project-euler.sol.p0070
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]
   [project-euler.lib.util :as util]))

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
     (loop [tpls (->> (prime/primes 11 (math/isqrt limit))
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
