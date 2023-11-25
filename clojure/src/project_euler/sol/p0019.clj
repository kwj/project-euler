(ns project-euler.sol.p0019)

(def ^:private days
  ;; days in month (Jan 1901 - *Nov* 2000)
  (let [common-year '(31 28 31 30 31 30 31 31 30 31 30 31)
        leap-year '(31 29 31 30 31 30 31 31 30 31 30 31)]
    (drop-last (flatten (repeat 25 (concat (repeat 3 common-year) leap-year))))))

(defn solve
  []
  ;; day of week - [0: Sunday, 1: Monday, 2: Tuesday, ..., 6: Saturday]
  ;; Jan 1, 1900 was Monday.
  (->> (reductions + (+ 1 365) days)
       (filter #(zero? (mod % 7)))
       (count)))

