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
         n-lower (my-math/pow 10 (dec n-digits))
         blk-upper-limit (my-math/pow 10 (* n-digits 2))
         blk-lower-limit (if (> n-digits 1) (my-math/pow 10 (* (dec n-digits) 2)) 0)
         blk-width (my-math/pow 10 (- (* n-digits 2) 2))]
     (loop [upper-lst (range blk-upper-limit blk-lower-limit (- blk-width))]
       (if (seq upper-lst)
         (let [blk-upper (first upper-lst)
               blk-lower (- blk-upper blk-width)
               prods (for [x (range n-upper (dec n-lower) -1)
                           :while (>= (* x x) blk-lower)
                           y (range (min (quot blk-upper x) x) (dec n-lower) -1)
                           :let [prod (* x y)]
                           :while (>= prod blk-lower)
                           :when (my-math/palindrome? prod)]
                       prod)]
           (if (seq prods)
             (apply max prods)
             (recur (next upper-lst))))
         (throw (ex-info "No answer is found" {:n-digits n-digits})))))))
