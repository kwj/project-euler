(ns project-euler.lib.math
  (:require
   [clojure.math]
   [project-euler.lib.util :as util]))

;;; Greatest Common Divisor (GCD) and Least Common Multiple (LCM)

(defn gcd
  "Compute the greatest common divisor by using Binary method (based on Knuth's TAOCP)."
  [^long a ^long b]
  {:pre [(and (int? a) (not= a Long/MIN_VALUE))
         (and (int? b) (not= b Long/MIN_VALUE))]}
  (let [a (abs a) b (abs b)]
    (cond
      (zero? a) b
      (zero? b) a
      :else (let [k (min (util/get-ntz a) (util/get-ntz b))]
              (loop [a (bit-shift-right a k)
                     b (bit-shift-right b k)
                     t (if (odd? a) (- b) (bit-shift-right a 1))]
                (let [x (bit-shift-right t (util/get-ntz (abs t)))]
                  (if (pos? x)
                    (if (zero? (- x b))
                      (bit-shift-left x k)
                      (recur x b (- x b)))
                    (if (zero? (- a x))
                      (bit-shift-left a k)
                      (recur a (abs x) (- a x))))))))))

(defn lcm
  "Compute the least common (positive) multiple (or zero if any argument is zero)."
  [^long a ^long b]
  {:pre [(and (int? a) (not= a Long/MIN_VALUE))
         (and (int? b) (not= b Long/MIN_VALUE))]}
  (cond
    (zero? a) 0
    (zero? b) 0
    :else (* (abs a) (/ (abs b) (gcd a b)))))

;;; GCD by using Extended Euclidean Algorithm

(defn gcdx
  "Compute the greatest common divisr and the coefficients of Bézout's identity."
  [^long a ^long b]
  {:pre [(and (int? a) (not= a Long/MIN_VALUE))
         (and (int? b) (not= b Long/MIN_VALUE))]}
  (letfn [(aux [^long a ^long b]
            (if (= a 0)
              [b 0 1]
              (let [[g x y] (aux (mod b a) a)]
                [g (- y (* (quot b a) x)) x])))]
    (let [sign_a (if (neg? a) -1 1)
          sign_b (if (neg? b) -1 1)
          [g x y] (aux (abs a) (abs b))]
      [g (* x sign_a) (* y sign_b)])))

;;; Power and Moduler exponentiation

(defn pow
  "Compute the exponentiation which base is `b` and power is `e`."
  [^long b ^long e]
  (let [sign (if (and (neg? b) (odd? e)) -1 1) inv (neg? e)]
    (loop [ret 1N b (bigint (abs b)) e (abs e)]
      (if (zero? e)
        (if inv
          (/ 1 (*' sign ret))
          (*' sign ret))
        (if (odd? e)
          (recur (*' ret b) (*' b b) (bit-shift-right e 1))
          (recur ret (*' b b) (bit-shift-right e 1)))))))

(defn invmod
  "Compute the modular multiplicative inverse `x` which `a` * `x` `mod` `m` = 1."
  [^long a ^long m]
  {:pre [(> m 1)]}
  (let [[g x _] (gcdx a m)]
    (if (= g 1)
      (mod x m)
      (assert false (str "greatest common divisor is " g ". (it must be one)")))))

(defn powermod
  "Compute the modular exponentiation which base is `b`, power is `p` and modulus is `m`."
  [^long b ^long e ^long m]
  {:pre [(>= m 0)]}
  (cond
    (= m 1) 0
    (zero? e) 1
    (neg? e) (powermod (invmod b m) (- e) m)
    :else (loop [ret 1 base (mod (abs b) m) e e]
            (if (zero? e)
              (if (neg? b)
                (- m ret)
                ret)
              (if (odd? e)
                (recur (long (mod (*' ret base) m)) (long (mod (*' base base) m)) (bit-shift-right e 1))
                (recur ret (long (mod (*' base base) m)) (bit-shift-right e 1)))))))

;;; Interger square root
;;;
;;; Port the implementation of the following URL.
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
  [^long n]
  {:pre [(pos? n)]}
  (let [tmp (inc (* 8 n)) tmp-sqrt (isqrt tmp)]
    (and (= (pow tmp-sqrt 2) tmp) (odd? tmp-sqrt))))

(defn square?
  "Check if a number `n` is square number."
  [^long n]
  {:pre [(pos? n)]}
  (let [tmp-sqrt (isqrt n)]
    (= (pow tmp-sqrt 2) n)))

(defn pentagonal?
  "Check if a number `n` is pentagonal number."
  [^long n]
  {:pre [(pos? n)]}
  (let [tmp (inc (* 24 n)) tmp-sqrt (isqrt tmp)]
    (and (= (pow tmp-sqrt 2) tmp) (= (mod tmp-sqrt 6) 5))))

(defn hexagonal?
  "Check if a number `n` is hexagonal number."
  [^long n]
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
  {:pre [(pos? n) (pos? k) (>= n k)]}
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
                (recur (quot n 10) (bit-or bits (bit-shift-left 1 (mod n 10))))
                bits)))]
    (= (mk-bits n) (dec (bit-shift-left 1 (num-of-digits n))))))

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
  "Factorial (return value is a long)."
  [^long n]
  {:pre [(>= n 0)]}
  (loop [x n, result 1N]
    (if (zero? x)
      result
      (recur (dec x) (*' result x)))))

(defn floor
  "Floor function (return value is a long)."
  [r]
  (long (clojure.math/floor r)))

(defn ceil
  "Ceil function."
  [r]
  (long (clojure.math/ceil r)))
