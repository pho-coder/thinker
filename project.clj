(defproject rocks.pho.btc/thinker "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [mount "0.1.11"]
                 [cprop "0.1.9"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ch.qos.logback/logback-classic "1.1.7"]
                 [org.clojure/tools.cli "0.3.5"]]
  :main rocks.pho.btc.thinker
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
