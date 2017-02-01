(ns rocks.pho.btc.thinker.utils
  (:require [clojure.data.json :as json]))

(defn parse-one-line
  [one-line]
  (json/read-str one-line
                 :key-fn keyword))

(defn get-readable-time
  "get readable time default format yyyy-MM-dd HH:mm:ss"
  ([long-time fm]
   (let [ts (java.sql.Timestamp. (if (= (.length (str long-time)) 10)
                                   (* long-time 1000)
                                   long-time))
         df (java.text.SimpleDateFormat. fm)]
     (.format df ts)))
  ([long-time]
   (get-readable-time long-time "yyyy-MM-dd HH:mm:ss.SSS")))
