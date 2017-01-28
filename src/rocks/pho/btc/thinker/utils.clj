(ns rocks.pho.btc.thinker.utils
  (:require [clojure.data.json :as json]))

(defn parse-one-line
  [one-line]
  (json/read-str one-line
                 :key-fn keyword))
