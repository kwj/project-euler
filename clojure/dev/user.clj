(ns user
  (:require
   [clojure.main :as main]
   [project-euler.core]))

(defn euler
  [& _]
  (set! *warn-on-reflection* true)
  (in-ns 'project-euler.core)
  (require '[project-euler.lib.math :as math])
  (require '[project-euler.lib.math.prime :as prime])
  (require '[project-euler.lib.util :as util])
  (println "Usage: (solve <problem number>)\n  example: (solve 1)"))
