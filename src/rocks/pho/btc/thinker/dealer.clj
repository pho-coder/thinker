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
        trend-lines (let [first-one (first dealed-info)
                          second-one (second dealed-info)
                          diff-price (- (:price second-one)
                                        (:price first-one))]
                      (loop [index 2
                             one-list (nthrest dealed-info 2)
                             highest-lowest-price {:highest-price (if (>= diff-price 0)
                                                                    (:price second-one)
                                                                    (:price first-one))
                                                   :highest-ts (if (>= diff-price 0)
                                                                 (:ts second-one)
                                                                 (:ts first-one))
                                                   :highest-index (if (>= diff-price 0)
                                                                    1
                                                                    0)
                                                   :lowest-price (if (<= diff-price 0)
                                                                   (:price second-one)
                                                                   (:price first-one))
                                                   :lowest-ts (if (<= diff-price 0)
                                                                (:ts second-one)
                                                                (:ts first-one))
                                                   :lowest-index (if (<= diff-price 0)
                                                                   1
                                                                   0)}
                             trend-lines (list {:start-index 0
                                                :start-price (:price first-one)
                                                :start-ts (:ts first-one)
                                                :end-index 1
                                                :end-price (:price second-one)
                                                :end-ts (:ts second-one)
                                                :trend (calc-up-down (:price second-one)
                                                                     (:price first-one))})]
                        (if (empty? one-list)
                          trend-lines
                          (let [one (first one-list)
                                last-line (last trend-lines)
                                trend (calc-up-down (:price one)
                                                    (:price (:start-price last-line)))
                                last-trend (:trend last-line)
                                start-trend (calc-up-down (:price one)
                                                          (:start-price last-line))
                                highest-trend (calc-up-down (:price one)
                                                            (:highest-price highest-lowest-price))
                                lowest-trend (calc-up-down (:price one)
                                                           (:lowest-price highest-lowest-price))]
                            ()))))
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
                                            )))))))
        trends2 (map #(assoc % :diff-price (- (:end-price %)
                                              (:start-price %))) trends)]
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
