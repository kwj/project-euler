(ns project-euler.sol.p0091
  (:require [project-euler.lib.math :as math]))

;;;;   0 <= x1,x2,y1,y2 <= 50
;;;;   P(x1,y1), Q(x2,y2)
;;;;
;;;;   case #1:
;;;;     1-1) the right angle is O(0,0)
;;;;       P(x1>0,0) and Q(0,y2>0)
;;;;       --> 50 * 50
;;;;     1-2) the right angle is on the x-axis [P(x1>0,y1=0)]
;;;;       P(x1>0,0) and Q(x2=x1,y2>0)
;;;;       --> 50 * 50
;;;;     1-3) the right angle is on the y-axis [Q(x1=0,y2>0)]
;;;;       P(x1>0,y1=y2) and Q(0,y2>0)
;;;;       --> 50 * 50
;;;;
;;;;   case #2:
;;;;     2-1) P(x1,y1) is the right angle and Qi(x_i>x1,y_i<y1)
;;;;       Y
;;;;        | P(x1,y1)
;;;;        |   #   Q1(a1,b1)
;;;;        |  .       *   Q2(a2,b2)
;;;;        | .               *
;;;;        |.                       *
;;;;       -O------------------------------------- X
;;;;                                             *
;;;;
;;;;       --> min((y1 / (x / gcd(x1,y1))), ((50-x1) / (y / gcd(x1,y1))))
;;;;
;;;;     2-2) P(x1,y1) is the right angle and Q(x2<x1,y2>y1)
;;;;       same qty as case 2-1.  [mirror on the y=x linne]

(defn- case-1
  [x-size y-size]
  (* x-size y-size 3))

(defn- case-2
  [x-size y-size]
  (->> (for [x (range 1 (inc x-size))
             y (range 1 (inc y-size))]
         (min (quot (* y (math/gcd x y)) x)
              (quot (* (- x-size x) (math/gcd x y)) y)))
       (apply +)
       (* 2)))

(defn solve
  ([]
   (solve 50 50))
  ([x-size y-size]
   (+ (case-1 x-size y-size)
      (case-2 x-size y-size))))
