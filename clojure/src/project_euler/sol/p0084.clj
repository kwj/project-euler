;;;; *WARNING*
;;;; This implementation returns a wrong answer when using 6-sided dice.
;;;; The cause has not been identified.

(ns project-euler.sol.p0084
  (:require [clojure.pprint]))

;;;;  Stochastic matrix: stoch-matrix
;;;;
;;;;         Next: S0       S1       S2
;;;;           +--------+--------+--------+ State:
;;;;  Current  |        |        |        |  S0: no doubles
;;;;   S0      |  s00   |  s01   | s02=0  |  S1: one doubles occurred
;;;;           |        |        |        |  S2: two consecutive dobules occurred
;;;;           +--------+--------+--------+
;;;;  Current  |        |        |        | s00: transition probability if no doubles occurred (the next state is S0)
;;;;   S1      |  s10   | s11=0  |  s12   | s01: transition probability if fist doubles occurred (the next state is S1)
;;;;           |        |        |        |
;;;;           +--------+--------+--------+ s10: transition probability if no doubles occurred (the next state is S0)
;;;;  Current  |        |        |        | s12: transition probability if two consecutive doubles occurred (the next state is S2)
;;;;   S2      |  s20   | s21=0  | s22=0  |
;;;;           |        |        |        | s20: transition probability if both doubles and no doubles occurred (the next state is S0)
;;;;           +--------+--------+--------+      Note: go to JAIL@S0 if three consecutive doubles occurred
;;;;
;;;;  s##:
;;;;         | GO  A1  CC1 ... [next square]
;;;;    -----+---------------------------
;;;;     GO  | ##  ##  ##  ...
;;;;     A1  | ##  ##  ##  ...
;;;;     CC1 | ##  ##  ##  ...
;;;;      .  |  .   .   .
;;;;  [current square]

;;; Squares
(def ^:private GO 0)
(def ^:private CC1 2)
(def ^:private R1 5)
(def ^:private CH1 7)
(def ^:private JAIL 10)
(def ^:private C1 11)
(def ^:private U1 12)
(def ^:private R2 15)
(def ^:private CC2 17)
(def ^:private CH2 22)
(def ^:private E3 24)
(def ^:private R3 25)
(def ^:private U2 28)
(def ^:private G2J 30)
(def ^:private CC3 33)
(def ^:private CH3 36)
(def ^:private H2 39)

(defn- make-dice-prblty-no-dbl
  [n-faces]
  (let [tmp (double-array (inc (* 2 n-faces)) 0)]
    (doseq [n (for [x (range 1 (inc n-faces))
                    y (range 1 (inc n-faces))
                    :when (not= x y)]
                (+ x y))]
      (aset tmp n (inc (aget tmp n))))
    (->> (seq tmp)
         (mapv #(/ % (* n-faces n-faces))))))

(defn- make-dice-prblty-dbl
  [n-faces]
  (let [tmp (double-array (inc (* 2 n-faces)) 0)]
    (doseq [n (for [x (range 1 (inc n-faces))
                    y (range 1 (inc n-faces))
                    :when (= x y)]
                (+ x y))]
      (aset tmp n (inc (aget tmp n))))
    (->> (seq tmp)
         (mapv #(/ % (* n-faces n-faces))))))

(defn- go-steady
  ^double/2 [^double/2 matrix]
  (loop [^double/2 prev matrix
         cnt 20] ; 2^20 = 1048576
    (if (zero? cnt)
      prev
      (let [^double/2 work (make-array Double/TYPE 120 120)]
        (doseq [x (range 120)
                y (range 120)
                :let [v (->> (range 120)
                             (map #(* (aget ^doubles (aget prev x) %)
                                      (aget ^doubles (aget prev %) y)))
                             (apply +))]]
          (aset ^doubles (aget work x) y (double v)))
        (recur work (dec cnt))))))

(defn solve
  ([]
   (solve 4))
  ([n-faces]
   (let [dice-prblty-no-dbl (make-dice-prblty-no-dbl n-faces)
         dice-prblty-dbl (make-dice-prblty-dbl n-faces)
         ^double/2 stoch-matrix (make-array Double/TYPE 120 120)]

     ;; No doubles occurred
     ;; -> setup 's00', 's10' and 's20'
     (doseq [[x v] (for [x (range 0 120)
                         v (range 2 (inc (* 2 n-faces)))]
                     [x v])]
       (aset ^doubles (aget stoch-matrix x) (mod (+ x v) 40) (double (get dice-prblty-no-dbl v))))

     ;; Dobules occrred
     ;; -> #1: setup 's01'
     (doseq [[x v] (for [x (range 0 40)
                         v (range 2 (inc (* 2 n-faces)))]
                     [x v])]
       (aset ^doubles (aget stoch-matrix x) (+ 40 (mod (+ x v) 40)) (double (get dice-prblty-dbl v))))
     ;; -> #2: setup 's12'
     (doseq [[x v] (for [x (range 40 80)
                         v (range 2 (inc (* 2 n-faces)))]
                     [x v])]
       (aset ^doubles (aget stoch-matrix x) (+ 80 (mod (+ x v) 40)) (double (get dice-prblty-dbl v))))
     ;; -> #3: setup 's20'
     (doseq [x (for [x (range 80 120)] x)]
       (aset ^doubles (aget stoch-matrix x)
             JAIL
             (double (+ (aget ^doubles (aget stoch-matrix x) JAIL)
                        (apply + dice-prblty-dbl)))))

     ;; Go to JAIL
     (doseq [[offset x] (for [offset [0 40 80]
                              x (range 0 120)]
                          [offset x])]
       (aset ^doubles (aget stoch-matrix x)
             JAIL
             (double (+ (aget ^doubles (aget stoch-matrix x) JAIL)
                        (aget ^doubles (aget stoch-matrix x) (+ G2J offset)))))
       (aset ^doubles (aget stoch-matrix x) (+ G2J offset) (double 0)))

     ;; Chance Card
     (doseq [[offset chance] (for [offset [0 40 80]
                                   chance [CH1 CH2 CH3]]
                               [offset chance])]
       (doseq [[current prblty] (zipmap (range 120) (aget stoch-matrix (+ chance offset)))]
         (let [next-r (if (= chance CH1) R2 (if (= chance CH3) R1 R3))
               next-u (if (= chance CH2) U2 U1)]
           (doseq [next-sq [GO C1 E3 H2 R1 next-r next-r next-u (mod (- chance 3) 40)]]
             (aset ^doubles (aget stoch-matrix current)
                   (+ next-sq offset)
                   (double (+ (aget ^doubles (aget stoch-matrix current) (+ next-sq offset))
                              (/ prblty 16.0)))))
           (aset ^doubles (aget stoch-matrix current)
                 JAIL
                 (double (+ (aget ^doubles (aget stoch-matrix current) JAIL)
                            (/ prblty 16.0))))
           (aset ^doubles (aget stoch-matrix current)
                 chance
                 (double (- (aget ^doubles (aget stoch-matrix current) chance)
                            (* (/ prblty 16.0) 10.0)))))))

     ;; Community Chest
     (doseq [[offset chest] (for [offset [0 40 80]
                                  chest [CC1 CC2 CC3]]
                              [offset chest])]
       (doseq [x (range 120)]
         (let [prblty (aget ^doubles (aget stoch-matrix x) (+ chest offset))]
           (aset ^doubles (aget stoch-matrix x)
                 (+ GO offset)
                 (double (+ (aget ^doubles (aget stoch-matrix x) (+ GO offset))
                            (/ prblty 16.0))))
           (aset ^doubles (aget stoch-matrix x)
                 (+ JAIL offset)
                 (double (+ (aget ^doubles (aget stoch-matrix x) (+ JAIL offset))
                            (/ prblty 16.0))))
           (aset ^doubles (aget stoch-matrix x)
                 (+ chest offset)
                 (double (- (aget ^doubles (aget stoch-matrix x) (+ chest offset))
                            (/ prblty 8.0)))))))

     (let [result (->> (aget ^double/2 (go-steady stoch-matrix) 0)
                       (vec)
                       (partition 40)
                       (apply mapv +))]
       (reduce (fn [acc [_ sq]] (format "%s%02d" acc sq))
               ""
               (take 3 (into (sorted-map-by >) (zipmap result (range 40)))))))))
