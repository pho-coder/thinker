(ns rocks.pho.btc.thinker.watcher
  (:require [mount.core :as mount]
            [clojure.tools.logging :as log]

            [rocks.pho.btc.thinker.utils :as utils]))

(mount/defstate last-top-point :start {:price 0
                                       :ts 0
                                       :datetime ""})

(defn check-detail
  [detail]
  (let [ts (System/currentTimeMillis)
        datetime (utils/get-readable-time ts)
        price (:p-new detail)
        format "YYYY-MM-dd_HH"]
    (when (>= price (:price last-top-point))
      (mount/start-with {#'last-top-point {:price price
                                           :ts ts
                                           :datetime datetime}}))
    (log/info "detail - new price:" (:p-new detail))))

(defn check-depth
  [depth]
  (let [format "YYYY-MM-dd_HH"]
    (log/info "one depth:\n"
              (str "asks-amount:\t\t"
                   (:asks-amount depth) "\n"
                   " bids-amount:\t\t" (:bids-amount depth) "\n"
                   " price1-asks-amount:\t" (:price1-asks-amount depth) "\n"
                   " price1-bids-amount:\t" (:price1-bids-amount depth) "\n"
                   " price2-asks-amount:\t" (:price2-asks-amount depth) "\n"
                   " price2-bids-amount:\t" (:price2-bids-amount depth)))))

(defn watch-once [one]
  (let [detail (:detail one)
        depth (:depth one)
        ts (:timestamp depth)
        datetime (utils/get-readable-time ts)]
    (check-detail detail)
    (check-depth depth)))
