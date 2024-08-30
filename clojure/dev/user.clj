(ns user
  (:require
   [project-euler.core]))

(defn euler
  [& _]
  (set! *warn-on-reflection* true)
  (in-ns 'project-euler.core)
  (println "Usage: (solve <problem number>)\n  example: (solve 1)"))
