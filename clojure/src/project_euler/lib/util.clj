(ns project-euler.lib.util
  (:require [clojure.java.io :as io]))

;;; Reaource reading

(defn read-data
  "Read a text resource and return a vector containg each line."
  [resource-name]
  (vec (line-seq (io/reader (io/resource resource-name)))))

;;; Naive implementations for permutation/combination

(defn- choose
  [lst]
  (letfn [(aux [left remaining]
            (if-not (seq remaining)
              '()
              (cons (list (first remaining)
                          (concat (reverse left) (rest remaining)))
                    (aux (cons (first remaining) left)
                         (rest remaining)))))]
    (aux '() lst)))

(defn permutation
  ([lst]
   (permutation (count lst) lst))
  ([^long n lst]
   (cond
     (zero? n) '(())
     (zero? (count lst)) '()
     :else (mapcat (fn [[x xs]] (map #(cons x %) (permutation (dec n) xs)))
                   (choose lst)))))

(defn permutation-with-repetition
  ([lst]
   (permutation-with-repetition (count lst) lst))
  ([^long n lst]
   (cond
     (zero? n) '(())
     (zero? (count lst)) '()
     :else (mapcat (fn [[x _]] (map #(cons x %) (permutation-with-repetition (dec n) lst)))
                   (choose lst)))))

(defn combination
  ([lst]
   (combination (count lst) lst))
  ([^long n lst]
   (cond
     (zero? n) '(())
     (zero? (count lst)) '()
     :else (lazy-cat (map #(cons (first lst) %) (combination (dec n) (rest lst)))
                     (combination n (rest lst))))))

(defn combination-with-repetition
  ([lst]
   (combination-with-repetition (count lst) lst))
  ([^long n lst]
   (cond
     (zero? n) '(())
     (zero? (count lst)) '()
     :else (lazy-cat (map #(cons (first lst) %) (combination-with-repetition (dec n) lst))
                     (combination-with-repetition n (rest lst))))))

;;; Naive implementation of cartesian product

(defn- cartesian-product-aux
  [colls]
  (if (empty? colls)
    '(())
    (let [coll (first colls)]
      (for [xs (cartesian-product-aux (rest colls))
            x coll]
        (cons x xs)))))

(defn cartesian-product
  "Cartesian Product.
  This function is used for readability (It can be substituted with the `for` macro)."
  [& colls]
  (when (every? seqable? colls)
    (map #(reverse %) (cartesian-product-aux (reverse colls)))))

;;; Miscellaneous functions

(defn get-ntz
  "Return a number of trailing zero bits to the right of the lowest order set bit."
  ^long [n]
  (Long/numberOfTrailingZeros n))

(defn digits
  "Return a lazy sequence of the digits of `n` in the given `base`."
  ([n]
   (digits n 10 1))
  ([n ^long base]
   (digits n base 1))
  ([n ^long base ^long pad]
   (loop [v (list (int (rem n base)))
          n (quot n base)]
     (if (= n 0)
       (concat (reverse v) (repeat (- pad (count v)) 0))
       (recur (conj v (int (rem n base))) (quot n base))))))

(defn undigits
  "Return an original number of the collection created by `digits`."
  ([v]
   (undigits v 10))
  ([v ^long base]
   (reduce #(+' (*' %1 base) %2) 0 (reverse v))))

(defn powerset
  [colls]
  (if (empty? colls)
    '(())
    (let [lst (powerset (rest colls))]
      (concat lst (map #(cons (first colls) %) lst)))))

(defn find-all
  [f lst]
  (keep-indexed #(if (f %2) %1 nil) lst))

