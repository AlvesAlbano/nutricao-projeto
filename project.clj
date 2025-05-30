(defproject nutricao-projeto "0.1.0-SNAPSHOT"
  :description "Projeto nutricional"
  :url "http://example.com/nutricao"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clj-http "3.13.0"]
                 [cheshire "6.0.0"]]
  :main ^:skip-aot nutricao-projeto.core
  :target-path "target/%s"
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dfile.encoding=UTF-8"]}})
