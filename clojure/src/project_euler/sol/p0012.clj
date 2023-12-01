(ns project-euler.sol.p0012
  (:require [project-euler.lib.math.prime :as prime]))

(def ^:private ndivs
  ;; "Return the number of divisors of `n`."
  (memoize (fn [n]
             (if (= n 1)
               1
               (->> (prime/factorize n)
                    (map #(inc (second %)))
                    (apply *))))))

(defn- ndivs-of-triangle
  "Return the number of divisors of the `i`-th triangular number."
  [ith]
  (if (odd? ith)
    (* (ndivs ith) (ndivs (quot (inc ith) 2)))
    (* (ndivs (quot ith 2)) (ndivs (inc ith)))))

(defn solve
  ([]
   (solve 500))
  ([thr]
   {:pre [(pos? thr)]}
   (let [x (first (drop-while #(<= (ndivs-of-triangle %) thr) (iterate inc 1)))]
     (quot (* x (inc x)) 2)))) ; triangle number's formula

