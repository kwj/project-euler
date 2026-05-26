(ns project-euler.sol.p0092
  (:require
   [project-euler.lib.math :as my-math]
   [project-euler.lib.util :as util]))

(defn- group89?
  [n]
  (loop [n n]
    (if (and (not= n 89) (> n 1))
      (recur (loop [x n
                    acc 0]
               (if (zero? x)
                 acc
                 (recur (quot x 10)
                        (+ acc (my-math/pow (mod x 10) 2))))))
      (= n 89))))

(defn solve
  ([]
   (solve 10000000))
  ([limit]
   {:pre [> (my-math/num-of-digits limit) (my-math/num-of-digits (dec limit))]}
   (let [n-digits (my-math/num-of-digits (dec limit))
         numerator (my-math/factorial n-digits)
         xf (comp (filter #(group89? (apply + %)))
                  (map #(reduce (fn [acc x] (* acc (my-math/factorial x))) 1 (vals (frequencies %))))
                  (map #(quot numerator %)))]
     (->> (range 0 10)
          (map #(* % %))
          (util/combination-with-repetition n-digits)
          (transduce xf +)
          (long)))))
