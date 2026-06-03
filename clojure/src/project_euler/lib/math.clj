(ns project-euler.lib.math
  (:require
   [clojure.math :as math]))

(defn get-ntz
  [n]
  (if (int? n)
    (Long/numberOfTrailingZeros n)
    (loop [n n
           cnt 0]
      (if (odd? n)
        cnt
        (recur (quot n 2) (inc cnt))))))

(defn bit-length
  [n]
  {:pre (>= n 0)}
  (loop [n n
         cnt 0]
    (if (zero? n)
      cnt
      (if (int? n)
        (recur (bit-shift-right n 1) (inc cnt))
        (recur (quot n 2) (inc cnt))))))

(defn bshift-right
  [n k]
  (if (int? n)
    (bit-shift-right n k)
    (loop [n n
           k k]
      (if (zero? k)
        n
        (recur (quot n 2) (dec k))))))

(defn bshift-left
  [n k]
  (if (int? n)
    (bit-shift-left n k)
    (loop [n n
           k k]
      (if (zero? k)
        n
        (recur (* n 2) (dec k))))))

(defn- possible-long
  [n]
  {:pre [(integer? n)]}
  (if (<= Long/MIN_VALUE n Long/MAX_VALUE) (long n) n))

;;; Greatest Common Divisor (GCD) and Least Common Multiple (LCM)

(defn gcd
  "Compute the greatest common divisor by using Binary method (based on Knuth's TAOCP)."
  [a b]
  {:pre [(integer? a) (integer? b)]}
  (let [a (abs a) b (abs b)]
    (cond
      (zero? a) b
      (zero? b) a
      :else (let [k (min (get-ntz a) (get-ntz b))]
              (loop [a (bshift-right a k)
                     b (bshift-right b k)
                     t (if (odd? a) (- b) (bshift-right a 1))]
                (let [x (bshift-right t (get-ntz (abs t)))]
                  (if (pos? x)
                    (if (zero? (- x b))
                      (possible-long (bshift-left x k))
                      (recur x b (- x b)))
                    (if (zero? (- a x))
                      (possible-long (bshift-left a k))
                      (recur a (abs x) (- a x))))))))))

(defn lcm
  "Compute the least common (positive) multiple (or zero if any argument is zero)."
  [a b]
  {:pre [(integer? a) (integer? b)]}
  (cond
    (zero? a) 0
    (zero? b) 0
    :else (* (abs a) (quot (abs b) (gcd a b)))))

;;; GCD by using Extended Euclidean Algorithm

(defn gcdx
  "Compute the greatest common divisr and the coefficients of Bézout's identity."
  [a b]
  {:pre [(integer? a) (integer? b)]}
  (letfn [(aux [a b]
            (if (= a 0)
              [b 0 1]
              (let [[g x y] (aux (mod b a) a)]
                [g (- y (* (quot b a) x)) x])))]
    (let [sign_a (if (neg? a) -1 1)
          sign_b (if (neg? b) -1 1)
          [g x y] (aux (abs a) (abs b))]
      [(possible-long g) (possible-long (* x sign_a)) (possible-long (* y sign_b))])))

;;; Power and Moduler exponentiation

(defn pow
  "Compute the exponentiation which base is `b` and power is `e`."
  [b e]
  {:pre [(integer? b) (integer? e)]}
  (let [sign (if (and (neg? b) (odd? e)) -1 1)
        inv (neg? e)]
    (loop [ret 1N
           b (bigint (abs b))
           e (abs e)]
      (if (zero? e)
        (if inv
          (/ 1 (* sign ret))
          (possible-long (* sign ret)))
        (if (odd? e)
          (recur (*' ret b) (*' b b) (bshift-right e 1))
          (recur ret (*' b b) (bshift-right e 1)))))))

(defn invmod
  "Compute the modular multiplicative inverse `x` which `a` * `x` `mod` `m` = 1."
  [a m]
  {:pre [(integer? a) (integer? m) (> m 1)]}
  (let [[g x _] (gcdx a m)]
    (if (= g 1)
      (possible-long (mod x m))
      (assert false (str "greatest common divisor is " g ". (it must be one)")))))

(defn powermod
  "Compute the modular exponentiation which base is `b`, power is `p` and modulus is `m`."
  [b e m]
  {:pre [(integer? b) (integer? e) (integer? m) (>= m 0)]}
  (cond
    (= m 1) 0
    (zero? e) 1
    (neg? e) (powermod (invmod b m) (- e) m)
    :else (loop [ret 1
                 base (mod (abs b) m)
                 e e]
            (if (zero? e)
              (if (neg? b)
                (possible-long (- m ret))
                (possible-long ret))
              (if (odd? e)
                (recur (mod (*' ret base) m) (mod (*' base base) m) (bshift-right e 1))
                (recur ret (mod (*' base base) m) (bshift-right e 1)))))))

;;; Interger square root

;;; Use clojure.math/sqrt

(defn isqrt-long
  "Integer square root for a long integer."
  ^long [^long n]
  (long (math/floor (math/sqrt n))))

;;; Porting from the implementation of the following URL.
;;;   https://github.com/mdickinson/snippets/blob/master/proofs/isqrt/src/isqrt.lean

(defn- isqrt-bit-length
  "Return the number of bits required to represent the number `n`."
  [n]
  (loop [n n c 0]
    (if (zero? n)
      c
      (recur (quot n 2) (inc c)))))

(defn- isqrt-aux
  [^long c n]
  (if (zero? c)
    1
    (let [k (quot (dec c) 2)
          a (isqrt-aux (quot c 2) (quot n (pow 2 (+ (* k 2) 2))))]
      (+' (*' a (pow 2 k)) (quot (quot n (pow 2 (+ k 2))) a)))))

(defn isqrt
  "Integer square root"
  [n]
  {:pre [(>= n 0)]}
  (cond
    (zero? n) 0
    :else (let [a (isqrt-aux (quot (dec (isqrt-bit-length n)) 2) n)]
            (if (< n (*' a a)) (dec a) a))))

;;; Polygonal numbers

(defmacro polygonal-numbers
  [name f]
  `(def ~name (reductions ~f 1 (iterate inc 1))))

(polygonal-numbers triangle-numbers (fn [x n] (+ x (inc n))))

(polygonal-numbers square-numbers (fn [x n] (+ x (inc (* 2 n)))))

(polygonal-numbers pentagonal-numbers (fn [x n] (+ x (inc (* 3 n)))))

(polygonal-numbers hexagonal-numbers (fn [x n] (+ x (inc (* 4 n)))))

(polygonal-numbers heptagonal-numbers (fn [x n] (+ x (inc (* 5 n)))))

(polygonal-numbers octagonal-numbers (fn [x n] (+ x (inc (* 6 n)))))

(defn triangular?
  "Check if a number `n` is triangular number."
  [n]
  {:pre [(pos? n)]}
  (let [tmp (inc (* 8 n)) tmp-sqrt (isqrt tmp)]
    (and (= (pow tmp-sqrt 2) tmp) (odd? tmp-sqrt))))

(defn square?
  "Check if a number `n` is square number."
  [n]
  {:pre [(pos? n)]}
  (let [tmp-sqrt (isqrt n)]
    (= (* tmp-sqrt tmp-sqrt) n)))

(defn pentagonal?
  "Check if a number `n` is pentagonal number."
  [n]
  {:pre [(pos? n)]}
  (let [tmp (inc (* 24 n)) tmp-sqrt (isqrt tmp)]
    (and (= (pow tmp-sqrt 2) tmp) (= (mod tmp-sqrt 6) 5))))

(defn hexagonal?
  "Check if a number `n` is hexagonal number."
  [n]
  {:pre [(pos? n)]}
  (let [tmp (inc (* 8 n)) tmp-sqrt (isqrt tmp)]
    (and (= (pow tmp-sqrt 2) tmp) (= (mod tmp-sqrt 4) 3))))

;;; Miscellaneous founctions

(defn get-max-exp
  "Return the largest exponent that does not exceed `n` when the base is `base`."
  [^long n ^long base]
  {:pre [(pos? n) (pos? base)]}
  (loop [n n e 0]
    (if (>= n base)
      (recur (quot n base) (inc e))
      e)))

(defn num-of-digits
  "Return digits of `n` when the base is `base`"
  ([^long n]
   {:pre [(pos? n)]}
   (num-of-digits n 10))
  ([^long n ^long base]
   {:pre [(pos? n) (pos? base)]}
   (inc (get-max-exp n base))))

(defn binomial
  "Binomial coefficient."
  [^long n ^long k]
  {:pre [(not (neg? n)) (not (neg? k)) (>= n k)]}
  (reduce #(quot (* %1 (get %2 0)) (get %2 1))
          1
          (map vector (range (inc (- n k)) (inc n)) (iterate inc 1))))

(defn palindrome?
  "Check whether the number `n` is palindrome or not."
  ([n]
   {:pre [(>= n 0)]}
   (palindrome? n 10))
  ([n ^long base]
   {:pre [(>= n 0) (pos? base)]}
   (loop [x n acc 0N]
     (if (pos? x)
       (recur (quot x base) (+' (*' acc base) (mod x base)))
       (= n acc)))))

(defn pandigital?
  "Check whether the number `n` is pandigital or not."
  [^long n]
  {:pre [(>= n 0)]}
  (letfn [(mk-bits [^long n]
            (loop [n n bits 0]
              (if (pos? n)
                (recur (quot n 10) (bit-or bits (bshift-left 1 (mod n 10))))
                bits)))]
    (= (mk-bits n) (dec (bshift-left 1 (num-of-digits n))))))

(defn pandigital-nz?
  "Check whether the number `n` is pandigital or not.
  Note: The number `n` doesn't contain 0."
  [^long n]
  {:pre [(pos? n)]}
  (letfn [(check-zero [^long n]
            (loop [n n]
              (if (pos? n)
                (if (zero? (mod n 10)) false (recur (quot n 10)))
                true)))]
    (and (check-zero n) (pandigital? (* n 10)))))

(defn factorial
  "Factorial (return value is a Long or BigInt).

  Note:
   20! = 2432902008176640000 (Long)
   21! = 51090942171709440000N (BigInt)
  "
  [^long n]
  {:pre [(>= n 0)]}
  (loop [x n, result 1]
    (if (zero? x)
      result
      (recur (dec x) (*' result x)))))

(defn jacobi-symbol
  [a n]
  {:pre [(pos? n) (odd? n)]}
  (loop [sign (if (and (neg? a) (= (mod n 4) 3)) -1 1)
         a (abs a)
         n n]
    (cond
      (zero? a) (if (= n 1) sign 0)
      (= a 1) sign
      (even? a) (let [ntz-a (get-ntz a)
                      next-a (bshift-right a ntz-a)]
                  (if (#{1 7} (mod n 8))
                    (recur sign next-a n)
                    (recur (if (odd? ntz-a) (- sign) sign) next-a n)))
      :else (let [rem-a (mod a n)]
              (if (even? rem-a)
                (recur sign rem-a n)
                (if (and (= (mod rem-a 4) 3) (= (mod n 4) 3))
                  (recur (- sign) n rem-a)
                  (recur sign n rem-a)))))))

(defn kronecker-symbol
  [a n]
  (if (zero? n)
    (if (= (abs a) 1) 1 0)
    (let [sign (if (and (neg? n) (neg? a)) -1 1)
          n (abs n)]
      (if (odd? n)
        (* sign (jacobi-symbol a n))
        (if (even? a)
          0
          (let [ntz-n (get-ntz n)
                js-result (jacobi-symbol a (bshift-right n ntz-n))]
            (if (#{3 5} (mod a 8))
              (* (if (odd? ntz-n) (- sign) sign) js-result)
              (* sign js-result))))))))
