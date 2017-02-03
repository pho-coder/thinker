(ns rocks.pho.btc.thinker.watcher
  (:require [mount.core :as mount]
            [clojure.tools.logging :as log]

            [rocks.pho.btc.thinker.utils :as utils]
            [rocks.pho.btc.thinker.config :refer [env]]))

(mount/defstate trade-point :start {:price 0
                                    :ts 0
                                    :datetime ""
                                    :type ""
                                    :amount 0})

(mount/defstate orders :start (list))

(mount/defstate wallet :start {:start-cny 0M
                               :cny 0M
                               :btc 0M
                               :ts 0
                               :datetime ""})

(mount/defstate last-top-point :start {:price 0
                                       :ts 0
                                       :datetime ""})

(mount/defstate recent-points :start (list))

(defn check-detail
  [detail ts]
  (let [datetime (utils/get-readable-time ts)
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

(defn buy
  [cny new-price ts]
  (let [datetime (utils/get-readable-time ts)]
    (log/info "buy:" cny)
    (mount/start-with {#'wallet {:start-cny (:start-cny wallet)
                                 :cny (- (:cny wallet)
                                         cny)
                                 :btc (with-precision 8 (/ cny new-price))
                                 :ts ts
                                 :datetime datetime}})
    (mount/start-with {#'trade-point {:price new-price
                                      :ts ts
                                      :datetime datetime
                                      :type "bid"
                                      :amount (:btc wallet)}})
    (mount/start-with {#'last-top-point {:price new-price
                                         :ts ts
                                         :datetime datetime}})
    (mount/start-with {#'orders (conj orders trade-point)})))

(defn sell
  [btc new-price ts]
  (let [datetime (utils/get-readable-time ts)]
    (log/info "sell:" btc)
    (mount/start-with {#'wallet {:start-cny (:start-cny wallet)
                                 :btc (- (:btc wallet)
                                         btc)
                                 :cny (+ (:cny wallet)
                                         (with-precision 8 (* new-price
                                                              (:btc wallet))))
                                 :ts ts
                                 :datetime datetime}})
    (log/info "maybe got diff:" (- new-price
                                   (:price trade-point)))
    (mount/start-with {#'trade-point {:price new-price
                                      :ts ts
                                      :datetime datetime
                                      :type "ask"
                                      :amount btc}})
    (mount/start-with {#'orders (conj orders trade-point)})))

(defn check-recent-points
  []
  (let [times (:check-times env)
        diff 2
        points recent-points]
    (if (< (.size points) times)
      (do (log/info "recent data is not enough:" times)
          "")
      (let [data (take times points)
            re (map (fn [o]
                      (let [asks-amount (:price2-asks-amount o)
                            bids-amount (:price2-bids-amount o)]
                        (if (>= (/ (double asks-amount)
                                   (double bids-amount))
                                diff)
                          "ask"
                          (if (>= (/ (double bids-amount)
                                     (double asks-amount))
                                  diff)
                            "bid"
                            ""))))
                    data)
            dealed (reduce (fn [t o]
                             (if (= t o)
                               t
                               "")) (first re) re)]
        (log/debug "recent data:" data)
        (log/info "recent re:" re)
        (when (or (and (= dealed "ask")
                       (< (:new-price (first data))
                          (- (:new-price (second data)) 2)))
                  (and (= dealed "bid")
                       (>= (:new-price (first data))
                           (:new-price (second data)))))
          (log/info "point:" dealed)
          dealed)))))

(defn init-wallet [cny ts]
  (mount/start-with {#'wallet {:start-cny (bigdec cny)
                                    :cny (bigdec cny)
                                    :btc 0M
                                    :ts ts
                                    :datetime (utils/get-readable-time ts)}})
  (log/info "finish init wallet:" wallet))

(defn watch-once [one]
  (let [detail (:detail one)
        depth (:depth one)
        ts (:timestamp depth)
        datetime (utils/get-readable-time ts)]
    (check-detail detail ts)
    (check-depth depth)
    (log/info "last top point:" last-top-point)

    ;; deal recent points
    (mount/start-with {#'recent-points (conj recent-points {:new-price (:p-new detail)
                                                            :asksed-amount (:ask-amount detail)
                                                            :bidsed-amount (:bid-amount detail)
                                                            :price1-asks-amount (:price1-asks-amount depth)
                                                            :price1-bids-amount (:price1-bids-amount depth)
                                                            :price2-asks-amount (:price2-asks-amount depth)
                                                            :price2-bids-amount (:price2-bids-amount depth)
                                                            :asks-amount (:asks-amount depth)
                                                            :bids-amount (:bids-amount depth)
                                                            :ts ts
                                                            :datetime datetime})})
    (while (> (.size recent-points) 100)
      (mount/start-with {#'recent-points (drop-last recent-points)}))

    ;; check recent points buy or sell
    (let [re (check-recent-points)
          btc (:btc wallet)
          cny (:cny wallet)]
      (when (and (= re "bid")
                 (<= btc 0))
        (log/info "CAN BUY")
        (buy (int cny) (:p-new detail) ts))
      (when (and (= re "ask")
                 (> btc 0))
        (log/info "CAN SELL")
        (sell btc (:p-new detail) ts)))
    
    ;; check must sell thousandth
    (when (> (:btc wallet) 0)
      (let [price (:p-new detail)
            diff-top (- (:price last-top-point) price)
            diff-time (/ (- ts (:ts last-top-point))
                         1000)
            diff-top-rate (/ (* diff-top 1000)
                             price)
            must-sell-rate (:must-sell-rate env)
            ;; cut must-sell-rate 50 quot, delay time no more than 52s
            sell-rate (* must-sell-rate (/ (- 52 diff-time) 50))]
        (when (> diff-top-rate
                 sell-rate)
          (log/info "MUST SELL")
          (log/info (str "\ndiff top more than " sell-rate "(" (with-precision 4 (bigdec sell-rate)) ") thousandth.")
                    "\ndiff time:" diff-time "(" (with-precision 4 (bigdec diff-time)) ")"
                    "\ndiff price:" diff-top "(" (with-precision 4 (bigdec diff-top)) ")"
                    "\nlast top point:" last-top-point)
          (sell (:btc wallet) (:p-new detail) ts))))

    (log/info "wallet:" wallet)))
