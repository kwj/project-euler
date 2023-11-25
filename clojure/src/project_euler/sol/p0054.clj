(ns project-euler.sol.p0054
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data
  [data]
  (->> (map #(str/split % #" ") data)
       (map (fn [cards] {:player-1 (take 5 cards) :player-2 (drop 5 cards)}))))

(defn- ch->num
  [ch]
  (case ch
    \2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14))

;;; I use this similar function because I don't know whether the public `indexOf` method of
;;; Clojure's PersistentVector/PersistentList classes can be used as public API.
(defn- index-of
  ^long [colls elm]
  (let [lst (util/find-all #(= elm %) colls)]
    (if (seq lst)
      (first lst)
      -1)))

(defn- flush?
  [suits]
  (= (count suits) 1))

(def ^:private straight-patterns
  (partition 5 1 (range 14 1 -1)))

(defn- straight?
  [nums]
  (case (index-of straight-patterns nums)
    -1 false
    true))

(def ^:private hand-pattern
  ;; 0: High Card - [1 1 1 1 1]
  ;; 1: One Pair [2 1 1 1]
  ;; 2: Two Pairs [2 2 1]
  ;; 3: Three of a Kind [3 1 1]
  ;; 4: Straight []  (Note: dummy pattern for index-of)
  ;; 5: Flush []  (Note: dummy pattern for index-of)
  ;; 6: Full House [3 2]
  ;; 7: Four of a Kind [4 1]
  '([1 1 1 1 1] [2 1 1 1] [2 2 1] [3 1 1] [] [] [3 2] [4 1]))

(defn- get-hand
  "Return a hand information.

  example: JH 2D JS QD AC -> [1 [11 14 12 2]]"
  [cards]
  (let [nums (->> (map first cards) (map ch->num) (sort) (reverse))
        suits (->> (map second cards) (set))
        pattern (->> (partition-by identity nums) (map count) (sort #(compare %2 %1)))
        hand-nums (->> (partition-by identity nums) (sort-by count #(compare %2 %1)) (map first) (vec))]
    (cond
      (flush? suits) (let [hand (index-of straight-patterns nums)]
                       (cond
                         (zero? hand) [9 hand-nums] ; 9: Royal Flush
                         (pos? hand) [8 hand-nums] ; 8: Straight Flush
                         :else [5 hand-nums])) ; 5: Flush
      (straight? nums) [4 hand-nums] ; 4: Straight
      :else [(index-of hand-pattern pattern) hand-nums]))) ; Others

(defn solve
  ([]
   (solve (util/read-data "0054_poker.txt")))
  ([data]
   (->> (parse-data data)
        (map #(compare (get-hand (:player-1 %)) (get-hand (:player-2 %))))
        (filter pos?)
        (count))))
