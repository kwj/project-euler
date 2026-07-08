(ns project-euler.sol.p0004
  (:require [project-euler.lib.math :as my-math]))

;;; The search area is divided into blocks, and search blocks in descending order
;;; until the answer is found.

(defn solve
  ([]
   (solve 3))
  ([n-digits]
   {:pre [(pos? n-digits)]}
   (let [n-upper (dec (my-math/pow 10 n-digits))
         n-lower (if (> n-digits 1) (my-math/pow 10 (dec n-digits)) 0)
         blk-width (my-math/pow 10 (- (* n-digits 2) 2))]
     (loop [blk-lower-lst (range (* (quot (* n-upper n-upper) blk-width) blk-width) (dec (* n-lower n-lower)) (- blk-width))]
       (if (seq blk-lower-lst)
         (let [blk-lower (first blk-lower-lst)
               blk-upper (dec (+ blk-lower blk-width))
               prods (for [x (range n-upper (dec n-lower) -1)
                           :while (>= (* x x) blk-lower)
                           y (range (if (> x 0) (min (quot blk-upper x) x) x) (dec n-lower) -1)
                           :let [prod (* x y)]
                           :while (>= prod blk-lower)
                           :when (my-math/palindrome? prod)]
                       prod)]
           (if (seq prods)
             (apply max prods)
             (recur (next blk-lower-lst))))
         (throw (ex-info "No answer is found" {:n-digits n-digits})))))))
