(ns project-euler.sol.p0068)

;;;;  ring: long array
;;;;
;;;;          r1
;;;;            \
;;;;            r0  [r6 <- r0]
;;;;           /  \
;;;;         r4---r2-r3
;;;;         /
;;;;       r5
;;;;
;;;;          r1
;;;;            \  [r10 <- r0]
;;;;            r0  r3
;;;;           /  \ /
;;;;         r8   r2
;;;;         /\   /
;;;;      r9 r6-r4-r5
;;;;            \
;;;;             r7

(def ^:private n-gon 5)

(def ^:private ring-work-size 11)

(defn- valid-numbers?
  [x y bit-mask]
  (and (< 0 x ring-work-size)
       (< 0 y ring-work-size)
       (not= x y)
       (not (bit-test bit-mask x))
       (not (bit-test bit-mask y))))

(defn- ring->str
  [ring]
  (loop [idx (- ring-work-size 3)
         s ""]
    (if (neg? idx)
      s
      (recur (- idx 2) (str (get ring (+ idx 1)) (get ring idx) (get ring (+ idx 2)) s)))))

;;; bit-mask: 1: used number, 0: unused number
;;;
;;; [when n-gon is equal to 5]
;;;
;;;  0x11111111110
;;;    ^        ^
;;;    10  ...  1
(defn- search-ring
  []
  (let [result (transient [])
        ring (apply vector (repeat ring-work-size 0))]
    (letfn [(dfs [idx bit-mask ring total]
              (let [start-pos 1]
                (if (= idx (- ring-work-size 3))
                  (let [tmp (- total (get ring 0) (get ring idx))]
                    (when (and (pos? tmp)
                               (< (get ring start-pos) tmp ring-work-size)
                               (not (bit-test bit-mask tmp)))
                      (conj! result (ring->str (assoc ring (+ idx 1) tmp)))))
                  (loop [nodes (range 1 ring-work-size)]
                    (when-first [outr-node nodes]
                      (let [inr-node (- total (get ring idx) outr-node)]
                        (when (valid-numbers? outr-node inr-node bit-mask)
                          (let [next-ring (assoc (assoc ring (+ idx 1) outr-node) (+ idx 2) inr-node)]
                            (when (<= (get next-ring start-pos) outr-node)
                              (dfs (+ idx 2)
                                   (bit-set (bit-set bit-mask outr-node) inr-node)
                                   next-ring
                                   total)))))
                      (recur (rest nodes)))))))]
      (loop [pairs (for [total (range (+ (* n-gon 2) 3) (inc (* n-gon 4)))
                         n (range 1 ring-work-size)]
                     [total n])]
        (when-first [[total n] pairs]
          (dfs 0 (bit-set 0 n) (assoc (assoc ring 0 n) (* n-gon 2) n) total)
          (recur (rest pairs))))
      (persistent! result))))

(defn solve
  ([]
   (solve 5))
  ([size]
   {:pre [(> size 2)]}
   (alter-var-root #'n-gon (constantly size))
   (alter-var-root #'ring-work-size (constantly (inc (* n-gon 2))))
   (let [rings (search-ring)]
     (cond
       (< n-gon 5) (->> (map parse-long rings)
                        (apply max))
       (= n-gon 5) (->> (filter #(= (count %) 16) rings)
                        (map parse-long)
                        (apply max))
       :else (->> (map biginteger rings)
                  (apply max))))))
