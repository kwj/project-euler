(ns project-euler.sol.p0076)

;;;; Another version of problem 31
;;;;
;;;;   coins: 1, 2, 3, ..., 99
;;;;   total: 100

(defn solve
  ([]
   (solve (range 1 100) 100))
  ([coins target]
   (let [tbl (long-array (inc target) 0)]
     (aset tbl 0 1)
     (doseq [c coins
             i (range c (inc target))]
       (aset tbl i (+ (aget tbl i) (aget tbl (- i c)))))
     (aget tbl target))))
