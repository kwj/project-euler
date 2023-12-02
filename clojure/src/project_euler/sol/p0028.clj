(ns project-euler.sol.p0028)

;;;;   21 22 23 24 25
;;;;   20  7  8  9 10
;;;;   19  6  1  2 11
;;;;   18  5  4  3 12
;;;;   17 16 15 14 13
;;;;          |  |  |  |
;;;;       (n=0, 1, 2, 3, ...)
;;;;
;;;;   the upper right number is:
;;;;     1    [n=0]
;;;;     (2n+1)**2    [n=>1]
;;;;
;;;;   so, the sum of numbers in the four corners is:
;;;;     (2n+1)**2 + ((2n+1)**2 - 2n) + ((2n+1)**2 - 4n) + ((2n+1)**2 - 6n)
;;;;       = 16n**2 + 4n + 4   [n>=1]
;;;;
;;;;   Answer: 1 + sum_{n=1}^{(1001-1)/2} (16n**2 + 4n + 4)

(defn solve
  ([]
   (solve 1001))
  ([len]
   {:pre [(> len 1) (odd? len)]}
   (->> (range 1 (inc (quot (dec len) 2)))
        (map #(+ (* 16 % %) (* 4 %) 4))
        (apply + 1))))
