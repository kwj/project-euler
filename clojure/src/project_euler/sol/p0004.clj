(ns project-euler.sol.p0004
  (:require [project-euler.lib.math :as math]))

;;; The search area is divided into blocks, and search blocks in descending order
;;; until the answer is found.

(defn solve
  ([]
   (solve 3))
  ([n-digits]
   {:pre [(pos? n-digits)]}
   (let [n-upper (dec (long (math/pow 10 n-digits)))
         n-lower (long (math/pow 10 (dec n-digits)))
         blk-upper-limit (long (math/pow 10 (* n-digits 2)))
         blk-lower-limit (if (> n-digits 1) (math/pow 10 (* (dec n-digits) 2)) 0)
         blk-width (long (math/pow 10 (- (* n-digits 2) 2)))]
     (loop [upper-lst (range blk-upper-limit blk-lower-limit (- blk-width))]
       (when-first [blk-upper upper-lst]
         (let [blk-lower (- blk-upper blk-width)
               prods (for [x (range n-upper (dec n-lower) -1)
                           y (range (min (quot blk-upper x) x) (dec n-lower) -1)
                           :let [prod (* x y)]
                           :when (and (>= (* x x) blk-lower)
                                      (>= prod blk-lower)
                                      (math/palindrome? prod))]
                       prod)]
           (if (seq prods)
             (apply max prods)
             (recur (rest upper-lst)))))))))
