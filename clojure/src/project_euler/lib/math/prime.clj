(ns project-euler.lib.math.prime
  (:require [project-euler.lib.math :as my-math]))

;;; Primality test
;;;
;;; First, test by trial division method, and then
;;;
;;; in case of
;;;   n < 2^16 - Lookup table
;;;   n < 2^32 - One-shot deterministic Miller–Rabin primality test (FJ32_256)
;;;              http://ceur-ws.org/Vol-1326/020-Forisek.pdf
;;;   n > 2^32 - Strengthened Baillie-PSW test
;;;              https://arxiv.org/abs/2006.14425v2

;; lookup table
(load "min_factors")

(def ^:private sprp-bases
  [15591 2018 166 7429 8064 16045 10503 4399 1949 1295 2776 3620 560 3128 5212 2657
   2300 2021 4652 1471 9336 4018 2398 20462 10277 8028 2213 6219 620 3763 4852 5012
   3185 1333 6227 5298 1074 2391 5113 7061 803 1269 3875 422 751 580 4729 10239
   746 2951 556 2206 3778 481 1522 3476 481 2487 3266 5633 488 3373 6441 3344
   17 15105 1490 4154 2036 1882 1813 467 3307 14042 6371 658 1005 903 737 1887
   7447 1888 2848 1784 7559 3400 951 13969 4304 177 41 19875 3110 13221 8726 571
   7043 6943 1199 352 6435 165 1169 3315 978 233 3003 2562 2994 10587 10030 2377
   1902 5354 4447 1555 263 27027 2283 305 669 1912 601 6186 429 1930 14873 1784
   1661 524 3577 236 2360 6146 2850 55637 1753 4178 8466 222 2579 2743 2031 2226
   2276 374 2132 813 23788 1610 4422 5159 1725 3597 3366 14336 579 165 1375 10018
   12616 9816 1371 536 1867 10864 857 2206 5788 434 8085 17618 727 3639 1595 4944
   2129 2029 8195 8344 6232 9183 8126 1870 3296 7455 8947 25017 541 19115 368 566
   5674 411 522 1027 8215 2050 6544 10049 614 774 2333 3007 35201 4706 1152 1785
   1028 1540 3743 493 4474 2521 26845 8354 864 18915 5465 2447 42 4511 1660 166
   1249 6259 2553 304 272 7286 73 6554 899 2816 5197 13330 7054 2818 3199 811
   922 350 7514 4452 3449 2663 4708 418 1621 1171 3471 88 11345 412 1559 194])

(defn- get-sprp-base
  [n]
  (let [h1 (biginteger n)
        h2 (.multiply (.xor (.shiftRight h1 (int 16)) h1) (biginteger 73244475)) ; 73244475 = 0x45d9f3b
        h3 (.multiply (.xor (.shiftRight h2 (int 16)) h2) (biginteger 73244475))
        idx (long (.and (.xor (.shiftRight h3 (int 16)) h3) (biginteger 255)))] ; 255 = 0xff
    (nth sprp-bases idx)))

(defn- nSPRP-test
  [n base]
  {:pre [(integer? n)]}
  (let [s (my-math/get-ntz (dec n))
        d (my-math/bshift-right (dec n) s)]
    (or (= (my-math/powermod base d n) 1)
        (boolean (some #(= % (dec n))
                       (map #(my-math/powermod base (* d (my-math/bshift-left 1 %)) n) (range s)))))))

(defn- lucas-seq-parameter
  "Return [D P Q] or nil by Method A*.

  If nil is returned, n is a composite number."
  [n]
  (loop [seq-d (map * (cycle [1 -1]) (iterate #(+ % 2) 5))
         thr 20]
    (let [d (first seq-d)
          k (my-math/jacobi-symbol d n)]
      (cond
        (= k -1) (if (= d 5)
                   [5 5 5]
                   [d 1 (quot (- 1 d) 4)])
        (and (zero? thr) (my-math/square? n)) nil
        (and (zero? k) (or (< (abs d) n) (not= (mod (abs d) n) 0))) nil
        :else (recur (next seq-d) (dec thr))))))

(defn- lucas-seq
  "Return Ud, Vd, Q^d and s.

  Note: n + 1 = d * 2^s"
  [n D P Q]
  (let [s (my-math/get-ntz (inc n))
        d (my-math/bshift-right (inc n) s)]
    (loop [Uk (biginteger 1)
           Vk (biginteger P)
           Qk (biginteger Q)
           idx (- (my-math/bit-length d) 2)]
      (if (neg? idx)
        [Uk Vk Qk s]
        (let [next-Uk (mod (* Uk Vk) n)
              next-Vk (mod (- (* Vk Vk) (* 2 Qk)) n)
              next-Qk (mod (* Qk Qk) n)]
          (if (odd? (my-math/bshift-right d idx))
            (let [tmp-Uk (+ (* P next-Uk) next-Vk)
                  tmp-Vk (+ (* D next-Uk) (* P next-Vk))]
              (recur (mod (my-math/bshift-right (if (odd? tmp-Uk) (+ tmp-Uk n) tmp-Uk) 1) n)
                     (mod (my-math/bshift-right (if (odd? tmp-Vk) (+ tmp-Vk n) tmp-Vk) 1) n)
                     (mod (* Q next-Qk) n)
                     (dec idx)))
            (recur next-Uk next-Vk next-Qk (dec idx))))))))

(defn- test-slprp
  "Check whether `n` is a strong Lucas Probable prime, slprp(P, Q).

  If yes, return [Vₙ₊₁ Q⁽ⁿ⁺¹⁾ᐟ²]. Otherwise, return `nil` because `n` is composite."
  [n D P Q]
  (letfn [(next-v [v q] (mod (- (* v v) (* 2 q)) n))
          (next-q [q] (mod (* q q) n))]
    (let [[Ud Vd Qd s] (lucas-seq n D P Q)
          ;; VQ-pairs: [[V_d, Q^d] [V_2d, Q^2d] ... [V_(2^(s-1))*d Q^(2^(s-1))*d]]
          VQ-pairs (take s (iterate (fn [[v q]] [(next-v v q) (next-q q)]) [Vd Qd]))]
      (if (or (zero? Ud) (boolean (some (fn [[v _]] (zero? v)) VQ-pairs)))
        ;; V_(2^s)*d = Vₙ₊₁  [because n + 1 = d * 2^s]
        ;; Q^(2^(s-1))*d = Q⁽ⁿ⁺¹⁾ᐟ²  [because d * 2^(s-1) = (d * 2^s)/2 = (n+1)/2]
        [(apply next-v (last VQ-pairs)) (last (last VQ-pairs))]
        nil))))

(defn- test-vprp
  "Check whether `n` is a Lucas-V probable prime, vprp(Q).

  If yes, return true. Otherwise, return false because `n` is composite."
  [Vₙ₊₁ Q n]
  (= (mod Vₙ₊₁ n) (mod (* 2 Q) n)))

(defn- test-euler-criterion
  "Return false if `n` is composite."
  [Q Q⁽ⁿ⁺¹⁾ᐟ² n]
  (or (= (Long/bitCount (abs Q)) 1) ; Skip if Q is a power of 2
      (= (mod Q⁽ⁿ⁺¹⁾ᐟ² n) (mod (* Q (my-math/jacobi-symbol Q n)) n))))

(defn- strengthened-BPSW-test
  [n]
  (and (nSPRP-test n 2) ; step 1
       (if-let [[D P Q] (lucas-seq-parameter n)] ; step 2
         (if-let [[Vₙ₊₁ Q⁽ⁿ⁺¹⁾ᐟ²] (test-slprp n D P Q)] ; step 3
           (and (test-vprp Vₙ₊₁ Q n) ; step 4
                (test-euler-criterion Q Q⁽ⁿ⁺¹⁾ᐟ² n)) ; step 5
           false)
         false)))

(defn prime?
  [n]
  {:pre [(integer? n)]}
  (cond
    (boolean (some #(= % n) [2 3 5 7])) true
    (boolean (some #(zero? (mod n %)) [2 3 5 7])) false
    (< n 121) (> n 1)
    (< n 65536) (= (nth min-factor-tbl (my-math/bshift-right n 1)) 1) ; 65536 = 2^16
    (< n 4294967296) (nSPRP-test n (get-sprp-base n)) ; 4294967296 = 2^32
    :else (strengthened-BPSW-test n)))

(defn fermat-prime?
  "Fermat primality test."
  [n]
  {:pre [(integer? n)]}
  (= (my-math/powermod 2 (dec n) n) 1))

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
  "A lazy sequence of prime numbers. (2 3 5 7 ...)"
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
      (doseq [i (range (inc (my-math/isqrt upper)))]
        (when (aget s-sieve i)
          (let [prime (index->num i)]
            (loop [idx (mod i 48)
                   q (my-math/pow prime 2)]
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
        (doseq [[i val] (map-indexed vector (get-small-prime-tbl (my-math/isqrt high)))]
          (when (true? val)
            (let [prime (index->num i)]
              (loop [idx (num->index (max (quot (dec (+ low prime)) prime) prime))
                     q (* prime (index->num idx))]
                (when (<= q max-prime)
                  (aset-boolean sieve (- (num->index q) w-low) false)
                  (recur (mod (inc idx) 48) (+ q (* prime (get wheel-spacing (mod idx 48)))))))))))
      sieve)))

(defn primes
  "Returns a vector of prime numbers in the specified range [`low`, `high`]."
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
     (persistent! v))))

(defn- factorize-aux
  [^long n ^long b]
  {:pre [(pos? n) (pos? b)]}
  (loop [n (quot n b)
         e 1]
    (if (zero? (mod n b))
      (recur (quot n b) (inc e))
      [n [b e]])))

(defn factorize
  "Return a vector of prime factorization of `n`.
  Each element is a vector of base and exponent,
  and the elements are ordered in ascending order of the bases.

  ```clojure
  => (factorize 168)
  [[2 3] [3 1] [7 1]]
  => (factorize 97)
  [[97 1]]
  ```"
  [^long n]
  {:pre [(pos? n)]}
  (if (== n 1)
    [[1 1]]
    (let [limit (my-math/isqrt n)]
      (loop [n n
             bs (take-while #(<= % limit) prime-numbers)
             v (transient [])]
        (if (== n 1)
          (persistent! v)
          (if-let [b (first bs)]
            (if (zero? (mod n b))
              (let [[next-n pair] (factorize-aux n b)]
                (recur (long next-n) (rest bs) (conj! v pair)))
              (recur n (rest bs) v))
            (persistent! (conj! v [n 1]))))))))

(defn divisors-from-pf
  "Make a sequence of divisors in ascending order from prime factorization.

  => (divisors-from-pf '([2 3] [3 1] [7 1]))
  (1 2 3 4 6 7 8 12 14 21 24 28 42 56 84 168)"
  [pf]
  (loop [prime-factorization pf v [1]]
    (if-let [[b e] (first prime-factorization)]
      (recur (rest prime-factorization)
             (into v (flatten (mapv #(mapv (partial * %) (mapv (partial my-math/pow b) (range 1 (inc e)))) v))))
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
            (let [next-x (long (+ x (my-math/pow q z)))]
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
