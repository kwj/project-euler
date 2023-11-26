(ns user
  (:require
   [clojure.main :as main]
   [project-euler.core]))

(defn dev-repl
  [& _]
  (set! *warn-on-reflection* true)
  (println "Usage: (solve <problem number>)\n  example: (solve 1)")
  (in-ns 'project-euler.core)
  (main/repl))
