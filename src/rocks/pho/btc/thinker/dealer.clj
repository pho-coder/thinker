(ns rocks.pho.btc.thinker.dealer)

(defn analysis-orders
  [orders]
  (let [first-bid (first orders)
        last-ask (last orders)]
    {:first-bid first-bid
     :last-ask last-ask
     :times (/ (.size orders) 2)}))

(defn prn-analysis-orders
  [orders]
  (let [re (analysis-orders orders)]
    (str "\n"
         "turns:\t" (:times re) "\n"
         "first bid:\t" (:first-bid re) "\n"
         "last ask:\t" (:last-ask re) "\n")))

(defn analysis-info
  [info]
  (let [first-detail (:detail (first info))
        first-depth (:depth (first info))
        last-detail (:detail (last info))
        last-depth (:depth (first info))]
    {:first-price (:p-new first-detail)
     :last-price (:p-new last-detail)
     :length (.size info)}))

(defn prn-analysis-info
  [info]
  (let [re (analysis-info info)]
    (str "\n"
         "length:\t" (:length re) "\n"
         "first price:\t" (:first-price re) "\n"
         "last price:\t" (:last-price re) "\n")))
