(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'project-euler)
(def version "0.1.0-SNAPSHOT")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

(defn clean
  [_]
  (b/delete {:path "target"}))

(defn uber
  [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :ns-compile '[project-euler.core]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'project-euler.core}))

