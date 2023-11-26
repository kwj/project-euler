(defproject project-euler "0.1.0-SNAPSHOT"
  :description "Project Euler solutions in Clojure"
  :url "https://github.com/kwj/project-euler"
  :license {:name "The MIT License"
            :url "https://opensource.org/license/mit/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :plugins [[dev.weavejester/lein-cljfmt "0.11.2"]]
  :main ^:skip-aot project-euler.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
