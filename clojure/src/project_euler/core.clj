(ns project-euler.core
  (:require [clojure.main :as main]
            project-euler.lib.math
            project-euler.lib.math.prime
            project-euler.lib.util)
  (:gen-class))

(defn solve
  "Run the solver for problem number `n`."
  [n]
  {:pre [(pos? n)]}
  (let [sol-sym (symbol (format "project-euler.sol.p%04d/solve" n))]
    (try
      (apply (requiring-resolve sol-sym) [])
      (catch java.io.FileNotFoundException _ (printf "There is no solver for problem %d.\n" n))
      (catch Exception e (println (str "Caught exception: " (.getMessage e)))))))

(def ^:private repl-options
  [:prompt #(printf "Enter a problem number: ")
   :read (fn [request-prompt request-exit]
           (or ({:line-start request-prompt :stream-end request-exit}
                (main/skip-whitespace *in*))
               (re-find #"\d+" (read-line))))
   :eval (fn [num-str]
           (time (solve (parse-long num-str))))])

(defn -main
  "Entry point."
  [& _]
  (apply main/repl repl-options))
