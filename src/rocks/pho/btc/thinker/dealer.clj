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

(defn calc-up-down
  [price last-price]
  (let [diff-price (- price last-price)
        threshold-price 2]
    (cond
      (< diff-price (- threshold-price)) "down"
      (> diff-price threshold-price) "up"
      :else "flat")))

(defn analysis-info
  [info]
  (let [first-detail (:detail (first info))
        first-depth (:depth (first info))
        last-detail (:detail (last info))
        last-depth (:depth (first info))
        dealed-info (map #(let [detail (:detail %)]
                            {:price (:p-new detail)
                             :ts (:timestamp detail)}) info)
        trends-list (loop [one-list (rest dealed-info)
                           last-one (first dealed-info)
                           re (list (assoc (first dealed-info)
                                           :trend "flat"))]
                      (if (empty? one-list)
                        re
                        (recur (rest one-list)
                               (first one-list)
                               (concat re (list (assoc (first one-list)
                                                       :trend (calc-up-down (:price (first one-list))
                                                                            (:price last-one))))))))
        trends (loop [one-list (rest trends-list)
                      last-one (first trends-list)
                      re (list {:start-price (:price (first trends-list))
                                :end-price (:price (first trends-list))
                                :trend (:trend (first trends-list))
                                :start-ts (:ts (first trends-list))
                                :end-ts (:ts (first trends-list))
                                :times 1})]
                 (if (empty? one-list)
                   re
                   (recur (rest one-list)
                          (first one-list)
                          (let [one (first one-list)
                                last-re (last re)]
                            (if (= (:trend one)
                                   (:trend last-one))
                              (concat (drop-last re) (list (assoc last-re
                                                                  :end-price (:price one)
                                                                  :end-ts (:ts one)
                                                                  :times (inc (:times last-re)))))
                              (concat re
                                      (list {:start-price (:price one)
                                             :end-price (:price one)
                                             :trend (:trend one)
                                             :start-ts (:ts one)
                                             :end-ts (:ts one)
                                             :times 1}
)))))))]
    {:first-price (:p-new first-detail)
     :last-price (:p-new last-detail)
     :length (.size info)
     :trends trends}))

(defn prn-analysis-info
  [info]
  (let [re (analysis-info info)]
    (str "\n"
         "length:\t" (:length re) "\n"
         "first price:\t" (:first-price re) "\n"
         "last price:\t" (:last-price re) "\n"
         "trends:\t" (:trends re) "\n")))
