(ns project-euler.sol.p0031)

(defn solve
  ([]
   (solve [1 2 5 10 20 50 100 200] 200))
  ([coins target]
   (let [tbl (long-array (inc target) 0)]
     (aset tbl 0 1)
     (doseq [c coins
             i (range c (inc target))]
       (aset tbl i (+ (aget tbl i) (aget tbl (- i c)))))
     (aget tbl target))))
