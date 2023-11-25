(ns project-euler.lib.math.prime
  (:require [project-euler.lib.math :as math]))

;;; Primality test
;;;
;;; Miller–Rabin primality test (Wikipedia based deterministic version under 2^64)
;;;  [Miller–Rabin primality test](https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test)
;;;  [Deterministic variants of the Miller-Rabin primality test](http://miller-rabin.appspot.com/)
;;;
;;; prime?

(defn- prime?-init
  [^long n]
  (loop [d (dec n) s 0]
    (if (odd? d)
      [d s]
      (recur (quot d 2) (inc s)))))

(defn- prime?-distinguish
  [^long a ^long d ^long s ^long n]
  (if (zero? (mod a n))
    ::Prime
    (loop [x (math/powermod a d n) cnt s]
      (if (zero? cnt)
        (if (not= x 1) ::Composite ::Undeceided)
        (let [y (math/powermod x 2 n)]
          (if (and (= y 1) (not= x 1) (not= x (dec n)))
            ::Composite
            (recur y (dec cnt))))))))

(defn prime?
  "Miller–Rabin primality test."
  [^long n]
  {:pre [(int? n)]}
  (if (or (< n 2) (and (not= (mod n 6) 5) (not= (mod n 6) 1)))
    (or (= n 2) (= n 3))
    (let [[d s] (prime?-init n)]
      (loop [xs [2 325 9375 28178 450775 9780504 1795265022]]
        (if-let [x (first xs)]
          (case (prime?-distinguish x d s n)
            ::Prime true
            ::Composite false
            ::Undeceided (recur (rest xs)))
          true)))))

;;; Lazy sequence of prime numbers
;;; https://en.wikipedia.org/wiki/Wheel_factorization
;;;
;;; prime-numbers

(def ^:private wheel-prime-candidates
  "Prime number candidates on the first wheel (the wheel range is from 11 to 220).
  There are 48 candidate numbers.
  Note: Not all are prime numbers. For example, `143` is a composite number.

  julia> cycle = 2 * 3 * 5 * 7
  julia> wheel_end = cycle + 10
  julia> lst = setdiff(1:wheel_end, 2:2:wheel_end, 3:3:wheel_end, 5:5:wheel_end, 7:7:wheel_end)
  julia> lst[2:end]"
  [11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71
   73  79  83  89  97  101 103 107 109 113 121 127 131 137 139 143
   149 151 157 163 167 169 173 179 181 187 191 193 197 199 209 211])

(def ^:private wheel-spacing
  "Spacing between prime number candidates on the wheel.

  julia> spacing = map(-, lst[2:end], lst)
  julia> push!(spacing[2:end], spacing[1])"
  [2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2
   6 4 6 8 4 2 4 2 4 8 6 4 6 2 4 6
   2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])

(def ^:private spacing-candidates
  (cycle wheel-spacing))

(defn- make-prime-candidates
  "Make a lazy sequence of prime number candidates."
  [n [x & xs]]
  (cons n (lazy-seq (make-prime-candidates (+ n x) xs))))

(defn- add-multiples
  "Insert multiples of `x` which are larger than or equal to `x` squared into the priority queue `pq`.
  Note that `x` is a prime number and the initial priority is `x` squared."
  [^"java.util.PriorityQueue" pq x xs]
  (.add pq [(*' x x) (map #(*' x %) xs)])
  pq)

(defn- sift-by-pq
  "Sift prime number candidates using a priority queue."
  ([[x & xs]]
   (let [pq (java.util.PriorityQueue. (fn [[a] [b]] (compare a b)))]
     (cons x (lazy-seq (sift-by-pq xs (add-multiples pq x xs))))))
  ([[x & xs] ^"java.util.PriorityQueue" pq]
   (let [lowest-composite-number (first (.peek pq))]
     (cond
       (> lowest-composite-number x) (cons x (lazy-seq (sift-by-pq xs (add-multiples pq x xs))))
       (= lowest-composite-number x) (do (while (= x (first (.peek pq)))
                                           (let [[_ [y & ys]] (.poll pq)]
                                             (.add pq [y ys])))
                                         (sift-by-pq xs pq))
       :else (do (println ">>> x =" x ", lowest composite =" lowest-composite-number)
                 (assert false "logical error"))))))

(defn- make-primes
  "Return a lazy sequence of prime numbers by sifting prime number candidates."
  [sift-fn]
  (concat [2 3 5 7] (sift-fn (make-prime-candidates 11 spacing-candidates))))

(def prime-numbers
  "A lazy sequnce of prime numbers. (2 3 5 7 ...)"
  (make-primes sift-by-pq))

;;; Functions related to prime numbers
;;;
;;; next-prime, prev-prime, primes
;;; factorize, divisors, proper-divisors
;;; make-sigma-tbl

(defn- num->index
  "Return the index indicating the lowest prime number candidate larger than or equal to `n`.

       ..=11:   0
     12..=13:   1
     14..=17:   2
     18..=19:   3
      <snip>
    198..=199: 45
    200..=209: 46
    210..=211: 47
    212..=221:  0 + 48
    222..=223:  1 + 48
    224..=227:  2 + 48

  The table `indices` was created by the following method.
  > (vec (flatten (concat (repeat (dec 11) 0)
                          (map-indexed #(repeat %2 (inc %1)) (drop-last wheel-spacing)))))"
  [^long n]
  {:pre [(int? n) (>= n 0)]}
  (if (< n 2)
    0
    (let [n (- n 2)
          indices [0  0  0  0  0  0  0  0  0  0  1  1  2  2  2
                   2  3  3  4  4  4  4  5  5  5  5  5  5  6  6
                   7  7  7  7  7  7  8  8  8  8  9  9  10 10 10
                   10 11 11 11 11 11 11 12 12 12 12 12 12 13 13
                   14 14 14 14 14 14 15 15 15 15 16 16 17 17 17
                   17 17 17 18 18 18 18 19 19 19 19 19 19 20 20
                   20 20 20 20 20 20 21 21 21 21 22 22 23 23 23
                   23 24 24 25 25 25 25 26 26 26 26 26 26 26 26
                   27 27 27 27 27 27 28 28 28 28 29 29 29 29 29
                   29 30 30 31 31 31 31 32 32 32 32 32 32 33 33
                   34 34 34 34 34 34 35 35 35 35 35 35 36 36 36
                   36 37 37 38 38 38 38 39 39 39 39 39 39 40 40
                   41 41 41 41 41 41 42 42 42 42 43 43 44 44 44
                   44 45 45 46 46 46 46 46 46 46 46 46 46 47 47]]
      (+ (* 48 (quot n 210)) (get indices (mod n 210))))))

(defn- index->num
  "Convert an index returned by `num->index` to a prime number candidate."
  [^long idx]
  {:pre [(int? idx) (>= idx 0)]}
  (let [q (quot idx 48)
        r (mod idx 48)]
    (+ (* 210 q) (get wheel-prime-candidates r))))

(defn next-prime
  "Return the lowest prime number which is larger than `n`."
  [^long n]
  {:pre [(int? n)]}
  (cond
    (< n 2) 2
    (< n 3) 3
    (< n 5) 5
    (< n 7) 7
    (< n 11) 11
    :else (loop [idx (inc (num->index n))]
            (let [p (index->num idx)]
              (if (prime? p) p (recur (inc idx)))))))

(defn prev-prime
  "Return the largest prime number which is smaller than `n`."
  [^long n]
  {:pre [(int? n) (> n 2)]}
  (cond
    (<= n 3) 2
    (<= n 5) 3
    (<= n 7) 5
    (<= n 11) 7
    :else (loop [idx (dec (num->index n))]
            (let [p (index->num idx)]
              (if (prime? p) p (recur (dec idx)))))))

(defn- get-small-prime-tbl
  [^long upper]
  {:pre [(>= upper 11)]}
  (let [max-index (dec (num->index (inc upper)))
        max-prime (index->num max-index)
        s-sieve (boolean-array (inc max-index) true)]
    (when (>= upper (* 11 11))
      (doseq [i (range (inc (math/isqrt upper)))]
        (when (aget s-sieve i)
          (let [prime (index->num i)]
            (loop [idx (mod i 48)
                   q (math/pow prime 2)]
              (when (<= q max-prime)
                (aset-boolean s-sieve (num->index q) false)
                (recur (mod (inc idx) 48) (+ q (* prime (get wheel-spacing idx))))))))))
    s-sieve))

(defn- get-prime-tbl
  [^long low ^long high]
  {:pre [(<= low high) (>= low 11)]}
  (if (= low 11)
    (get-small-prime-tbl high)
    (let [w-low (num->index low)
          w-high (dec (num->index (inc high)))
          max-prime (index->num w-high)
          sieve (boolean-array (inc (- w-high w-low)) true)]
      (when (>= high (* 11 11))
        (doseq [[i val] (map-indexed vector (get-small-prime-tbl (math/isqrt high)))]
          (when (true? val)
            (let [prime (index->num i)]
              (loop [idx (num->index (max (quot (dec (+ low prime)) prime) prime))
                     q (* prime (index->num idx))]
                (when (<= q max-prime)
                  (aset-boolean sieve (- (num->index q) w-low) false)
                  (recur (mod (inc idx) 48) (+ q (* prime (get wheel-spacing (mod idx 48)))))))))))
      sieve)))

(defn primes
  "Returns a sequence of prime numbers in the specified range [`low`, `high`]."
  ([^long high]
   {:pre [(int? high) (>= high 2)]}
   (primes 1 high))
  ([^long low ^long high]
   {:pre [(int? low) (int? high) (pos? high) (<= low high)]}
   (let [v (transient [])]
     (when (<= low 2 high)
       (conj! v 2))
     (when (<= low 3 high)
       (conj! v 3))
     (when (<= low 5 high)
       (conj! v 5))
     (when (<= low 7 high)
       (conj! v 7))
     (when (>= high 11)
       (let [low (max low 11)
             w-offset (num->index low)]
         (doseq [[i val] (map-indexed vector (get-prime-tbl low high))]
           (when (true? val)
             (conj! v (index->num (+ i w-offset)))))))
     (seq (persistent! v)))))

(defn- factorize-aux
  [^long n ^long b]
  {:pre [(pos? n) (pos? b)]}
  (loop [n n
         e 0]
    (if (zero? (mod n b))
      (recur (quot n b) (inc e))
      (if (zero? e)
        [n []]
        [n [b e]]))))

(defn factorize
  "Return a sequence of prime factorization of `n`.
  Each element is a vector of base and exponent.

  => (factorize 168)
  ([2 3] [3 1] [7 1])
  => (factorize 97)
  ([97 1])"
  [^long n]
  {:pre [(pos? n)]}
  (if (== n 1)
    (seq [[1 1]])
    (let [limit (math/isqrt n)
          v (transient [])]
      (loop [n n
             bs (take-while #(<= % limit) (concat [2 3 5 7] (make-prime-candidates 11 spacing-candidates)))]
        (if (== n 1)
          (seq (persistent! v))
          (if-let [b (first bs)]
            (let [[next-n pair] (factorize-aux n b)]
              (when (seq pair)
                (conj! v pair))
              (recur (long next-n) (rest bs)))
            (seq (conj (persistent! v) [n 1]))))))))

(defn divisors-from-pf
  "Make a sequence of divisors in ascending order from prime factorization.

  => (divisors-from-pf '([2 3] [3 1] [7 1]))
  (1 2 3 4 6 7 8 12 14 21 24 28 42 56 84 168)"
  [pf]
  (loop [prime-factorization pf v [1]]
    (if-let [[b e] (first prime-factorization)]
      (recur (rest prime-factorization)
             (into v (flatten (mapv #(mapv (partial * %) (mapv (partial math/pow b) (range 1 (inc e)))) v))))
      (sort v))))

(defn divisors
  "Return a sequence of divisors of `n` in ascending order.

  => (divisors 168)
  (1 2 3 4 6 7 8 12 14 21 24 28 42 56 84 168)
  => (divisors 97)
  (1 97)"
  [^long n]
  {:pre [(int? n) (pos? n)]}
  (divisors-from-pf (factorize n)))

(defn proper-divisors
  "Return a sequence of proper divisors of `n`."
  [^long n]
  {:pre [(int? n) (pos? n)]}
  (butlast (divisors n)))

(defn make-sigma-tbl
  "Return the result of the divisor function calculation as a vector.
  https://en.wikipedia.org/wiki/Divisor_function"
  [^long z ^long upper]
  (let [result (long-array (inc upper) 1)]
    (loop [prime-lst (take-while #(< % upper) prime-numbers)]
      (when-first [p prime-lst]
        (loop [q p, x 0]
          (when (<= q upper)
            (let [next-x (long (+ x (math/pow q z)))]
              (aset-long result q (+ (aget ^longs result q) next-x))
              (recur (* q p) next-x))))
        (recur (rest prime-lst))))
    (loop [prime-lst (take-while #(< % upper) prime-numbers)]
      (when-first [p prime-lst]
        (loop [q p]
          (when (<= q upper)
            (loop [ns (range 2 (inc (quot upper q)))]
              (when-first [n ns]
                (when (and (not= (aget ^longs result n) 1) (not (zero? (mod n p))))
                  (aset-long result (* q n) (* (aget ^longs result q) (aget ^longs result n))))
                (recur (rest ns))))
            (recur (* q p))))
        (recur (rest prime-lst))))
    (aset-long result 0 0)
    (vec result)))
