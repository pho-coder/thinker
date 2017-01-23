(ns rocks.pho.btc.thinker
  (:require [clojure.tools.logging :as log]
            [mount.core :as mount]
            [clojure.tools.cli :refer [parse-opts]]

            [rocks.pho.btc.thinker.config :refer [env]]
            [rocks.pho.btc.thinker.watcher :as watcher])
  (:gen-class))

(def cli-options
  [["-f" "--file file-path" "file path"
    :parse-fn #(clojure.java.io/as-file %)
    :validate [#(.exists %) "Must be a file"]]])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (log/info "Hello, btc thinker!")
  (doseq [component (-> args
                        mount/start-with-args
                        :started)]
    (log/info component "started"))
  (let [opts (parse-opts args cli-options)
        options (:options opts)]
    (when-not (nil? (:errors opts))
      (log/error opts)
      (System/exit 1))
    (let [file (:file options)]
      (log/info file))))
