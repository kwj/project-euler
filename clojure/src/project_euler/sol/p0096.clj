(ns project-euler.sol.p0096
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

;;;
;;;  This solution implements a naive backtracking algorithm,
;;;  so it is not fast.
;;;
;;;      C0 C1 C2 C3 C4 C5 C6 C7 C8
;;;     +--------+--------+--------+    R: Row
;;;  R0 | 0  1  2| 3  4  5| 6  7  8|    C: Column
;;;  R1 | 9 10 11|12      |15      |
;;;  R2 |18      |21      |24      |    Cell: 0..=80
;;;     +--------+--------+--------+
;;;  R3 |27      |30      |33      |
;;;  R4 |36      |39      |42      |
;;;  R5 |45      |48      |51      |
;;;     +--------+--------+--------+
;;;  R6 |54      |57      |60      |
;;;  R7 |63      |66      |69      |
;;;  R8 |72      |75 76 77|78 79 80|
;;;     +--------+--------+--------+
;;;
;;;  grid: [BigInteger; 10]
;;;    grid[n] is the information of number 'n'
;;;
;;;         81 80                   0
;;;     ......|.....................|
;;;     unused       bit table
;;;
;;;  Example:
;;;   * Grid 01 (from 0096_sudoku.txt)
;;;      003|020|600
;;;      900|305|001
;;;      001|806|400
;;;      ---+---+---
;;;      008|102|900
;;;      700|000|008
;;;      006|708|200
;;;      ---+---+---
;;;      002|609|500
;;;      800|203|009
;;;      005|010|300
;;;
;;;    Initial Data: grid[]
;;;      [0x1ab6b64f26fec9e4dadab,
;;;       0x010000000000040120000, 0x000040108000100000010, 0x040100000000000001004,
;;;       0x000000000000001000000, 0x004001000000000004000, 0x000000200800000800040,
;;;       0x000000001001000000000, 0x000008004100020200000, 0x000800800000200000200]
;;;
;;;    1) number '0'
;;;        python> bin(grid[0])[::-1][0:10]    # print only 'R0'
;;;        '110101011'
;;;
;;;        R0: 003|020|600
;;;           '110 101 011'
;;;
;;;    2) number '3'
;;;        python> bin(grid[3])[::-1][0:10]    # print only 'R0'
;;;        '001000000'
;;;
;;;        R0: 003|020|600
;;;           '001 000 000'
;;;
;;;   * Grid 01 (Solution)
;;;      483|921|657
;;;      967|345|821
;;;      251|876|493
;;;      ---+---+---
;;;      548|132|976
;;;      729|564|138
;;;      136|798|245
;;;      ---+---+---
;;;      372|689|514
;;;      814|253|769
;;;      695|417|382
;;;
;;;    Solution: sol[]
;;;      [0x1ab6b64f26fec9e4dadab,
;;;       0x010012000240040120020, 0x100040108002100050010, 0x040100040480084001004,
;;;       0x008024010020011002001, 0x004081020008008084080, 0x001400200810800800440,
;;;       0x020200081001400400900, 0x080008404100020208002, 0x002800802004202000208]
;;;
;;;    1) number '4'
;;;      python> "{:081b}".format(sol[4])[::-1]
;;;     '100000000000010000000000100010000000000001000000000010000000001001000000000100000'
;;;      python> s = "{:081b}".format(sol[4])[::-1]
;;;      python> for i in range(0, len(s), 9):
;;;                  print(s[i:i+9])
;;;
;;;      100000000
;;;      000010000
;;;      000000100
;;;      010000000
;;;      000001000
;;;      000000010
;;;      000000001
;;;      001000000
;;;      000100000
;;;
;;;        The set bits indicate that there is a number at position. In the above case, the number is 4.
;;;        Please compare it with the solution of Grid 01.

(def ^:private total-cells 81)

(def ^:private ^"[Ljava.math.BigInteger;" adjacent-cells
  (let [^"[Ljava.math.BigInteger;" tbl (make-array java.math.BigInteger total-cells)
        ;; 0b: 111111111
        row-mask (java.math.BigInteger. 1 (byte-array [0x01 0xFF]))
        ;; 0b: 000000001 000000001 ... 000000001
        col-mask (java.math.BigInteger. 1 (byte-array [0x01, 0x00, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01]))
        ;; 0b: 000000111 000000111 000000111
        box-mask (java.math.BigInteger. 1 (byte-array [0x1c, 0x0e, 0x07]))]
    (doseq [pos (range total-cells)]
      (let [row-shift (* (quot pos 9) 9)
            col-shift (mod pos 9)
            box-shift (+ (* (quot pos 27) 27) (* (quot col-shift 3) 3))]
        (aset tbl pos (-> (java.math.BigInteger. 0 (byte-array [0]))
                          (.or ^java.math.BigInteger (.shiftLeft row-mask row-shift))
                          (.or ^java.math.BigInteger (.shiftLeft col-mask col-shift))
                          (.or ^java.math.BigInteger (.shiftLeft box-mask box-shift))))))
    tbl))

(defn- process-data
  [rows]
  (let [s (-> (str/join rows)
              (str/replace #"[^0-9.]" "")
              (str/replace #"\." "0"))]
    (->> (map #(- (int %) (int \0)) s)
         (vec))))

(defn- parse-data
  [data]
  (loop [xs data
         acc []
         result (transient [])]
    (if (seq xs)
      (let [s (first xs)]
        (cond
          (re-find #"^[0-9.]" s) (recur (rest xs) (conj acc s) result)
          (re-find #"^-" s) (recur (rest xs) acc result)
          (not (zero? (count acc)))  (recur (rest xs) [] (conj! result (process-data acc)))
          :else (recur (rest xs) acc result)))
      (do (when (not (zero? (count acc)))
            (conj! result (process-data acc)))
          (persistent! result)))))

(defn- make-grid
  ^"[Ljava.math.BigInteger;" [puzzle]
  (if-not (= (count puzzle) total-cells)
    (do (println "[Warning] The size of input data is mismatch.")
        (println "  Skipped:" puzzle))
    (let [^"[Ljava.math.BigInteger;" grid (make-array java.math.BigInteger 10)]
      (doseq [i (range 10)]
        (aset grid i (java.math.BigInteger. 0 (byte-array [0]))))
      (loop [xs (map-indexed vector puzzle)]
        (if-let [[pos n] (first xs)]
          (if (or (zero? n)
                  (zero? (.and ^java.math.BigInteger (aget adjacent-cells pos) (aget grid n))))
            (do (aset grid n (.setBit ^java.math.BigInteger (aget grid n) pos))
                (recur (next xs)))
            (do (println "[Warning] there is a same number in the adjacent cells.")
                (println "  Skipped:" puzzle)))
          grid)))))

(defn- sudoku-solver-aux
  [^"[Ljava.math.BigInteger;" grid pos]
  (letfn [(search-undetermined-cell [i]
            (loop [i i]
              (cond
                (>= i total-cells) i
                (.testBit ^java.math.BigInteger (aget grid 0) i) i
                :else (recur (inc i)))))]
    (let [pos (search-undetermined-cell pos)]
      (if (>= pos total-cells)
        true
        (loop [ns (range 1 10)]
          (if-let [n (first ns)]
            (if (zero? (.and ^java.math.BigInteger (aget grid n) (aget adjacent-cells pos)))
              (do (aset grid n (.setBit ^java.math.BigInteger (aget grid n) pos))
                  (if (sudoku-solver-aux grid (inc pos))
                    true
                    (do (aset grid n (.clearBit ^java.math.BigInteger (aget grid n) pos))
                        (recur (next ns)))))
              (recur (next ns)))
            false))))))

(defn- sudoku-solver
  [^"[Ljava.math.BigInteger;" grid]
  (let [^"[Ljava.math.BigInteger;" work-grid (make-array java.math.BigInteger 10)]
    (doseq [i (range 10)]
      (aset work-grid i (java.math.BigInteger. (.toString (aget grid i)))))
    (if (sudoku-solver-aux work-grid 0)
      work-grid
      nil)))

(defn- get-3-digit-number
  [^"[Ljava.math.BigInteger;" sol]
  (loop [is (range 3)
         acc 0]
    (if (seq is)
      (let [i (first is)]
        (recur (next is)
               (long (+ (* acc 10)
                        (->> (map-indexed vector sol)
                             (drop 1)
                             (drop-while #(not (.testBit ^java.math.BigInteger (second %) i)))
                             (ffirst))))))
      acc)))

(defn solve
  ([]
   (solve (util/read-data "0096_sudoku.txt")))
  ([data]
   (->> (parse-data data)
        (map make-grid)
        (map sudoku-solver)
        (map get-3-digit-number)
        (apply +))))
