(ns rocks.pho.btc.thinker.config
  (:require [mount.core :refer [defstate]]
            [cprop.source :as source]))

(defstate env :start (source/from-env))
