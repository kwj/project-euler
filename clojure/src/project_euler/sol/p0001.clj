(ns project-euler.sol.p0001)

(defn solve
  ([]
   (solve 1000))
  ([upper]
   (letfn [(f [x]
             (let [tmp (quot (dec upper) x)]
               (quot (* (inc tmp) tmp x) 2)))]
     (- (+ (f 3) (f 5)) (f 15)))))

