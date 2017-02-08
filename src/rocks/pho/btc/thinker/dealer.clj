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
                          change-highest-lowest-price (fn [highest-lowest-price new-one new-index]
                                                        (let [new-price (:price new-one)
                                                              new-ts (:ts new-one)]
                                                          (cond
                                                            (>= new-price (:highest-price highest-lowest-price))
                                                            (assoc highest-lowest-price
                                                                   :highest-price new-price
                                                                   :highest-ts new-ts
                                                                   :highest-index new-index)
                                                            (<= new-price (:lowest-price highest-lowest-price))
                                                            (assoc highest-lowest-price
                                                                   :lowest-price new-price
                                                                   :lowest-ts new-ts
                                                                   :lowest-index new-index)
                                                            :else
                                                            highest-lowest-price)))]
                      (loop [index 1
                             one-list (rest dealed-info)
                             highest-lowest-price {:highest-price (:price first-one)
                                                   :highest-ts (:ts first-one)
                                                   :highest-index 0
                                                   :lowest-price (:price first-one)
                                                   :lowest-ts (:ts first-one)
                                                   :lowest-index 0}
                             trend-lines (list {:start-index 0
                                                :start-price (:price first-one)
                                                :start-ts (:ts first-one)
                                                :end-index 0
                                                :end-price (:price first-one)
                                                :end-ts (:ts first-one)
                                                :trend (calc-up-down (:price first-one)
                                                                     (:price first-one))})]
                        (if (empty? one-list)
                          trend-lines
                          (let [one (first one-list)
                                last-line (last trend-lines)
                                last-trend (:trend last-line)
                                highest-trend (calc-up-down (:price one)
                                                            (:highest-price highest-lowest-price))
                                lowest-trend (calc-up-down (:price one)
                                                           (:lowest-price highest-lowest-price))]
                            (cond
                              (= lowest-trend "up") (if (= last-trend "up")
                                                      (recur (inc index)
                                                             (rest one-list)
                                                             (change-highest-lowest-price highest-lowest-price one index)
                                                             (concat (drop-last trend-lines)
                                                                     (list (assoc last-line
                                                                                  :end-index index
                                                                                  :end-price (:price one)
                                                                                  :end-ts (:ts one)))))
                                                      (recur (inc index)
                                                             (rest one-list)
                                                             (change-highest-lowest-price highest-lowest-price one index)
                                                             (concat (concat (drop-last trend-lines)
                                                                             (list (assoc last-line
                                                                                          :end-index (:lowest-index highest-lowest-price)
                                                                                          :end-price (:lowest-price highest-lowest-price)
                                                                                          :end-ts (:lowest-ts highest-lowest-price))))
                                                                     (list {:start-index (:lowest-index highest-lowest-price)
                                                                            :start-price (:lowest-price highest-lowest-price)
                                                                            :start-ts (:lowest-ts highest-lowest-price)
                                                                            :end-index index
                                                                            :end-price (:price one)
                                                                            :end-ts (:ts one)
                                                                            :trend "up"}))))
                              (= highest-trend "down") (if (= last-trend "down")
                                                         (recur (inc index)
                                                                (rest one-list)
                                                                (change-highest-lowest-price highest-lowest-price one index)
                                                                (concat (drop-last trend-lines)
                                                                        (list (assoc last-line
                                                                                     :end-index index
                                                                                     :end-price (:price one)
                                                                                     :end-ts (:ts one)))))
                                                         (recur (inc index)
                                                                (rest one-list)
                                                                (change-highest-lowest-price highest-lowest-price one index)
                                                                (concat (concat (drop-last trend-lines)
                                                                                (list (assoc last-line
                                                                                             :end-index (:highest-index highest-lowest-price)
                                                                                             :end-price (:highest-price highest-lowest-price)
                                                                                             :end-ts (:highest-ts highest-lowest-price))))
                                                                        (list {:start-index (:highest-index highest-lowest-price)
                                                                               :start-price (:highest-price highest-lowest-price)
                                                                               :start-ts (:highest-ts highest-lowest-price)
                                                                               :end-index index
                                                                               :end-price (:price one)
                                                                               :end-ts (:ts one)
                                                                               :trend "down"}))))
                              :else (recur (inc index)
                                           (rest one-list)
                                           (change-highest-lowest-price highest-lowest-price one index)
                                           (concat (drop-last trend-lines)
                                                   (list (assoc last-line
                                                                :end-index index
                                                                :end-price (:price one)
                                                                :end-ts (:ts one)
                                                                :trend (:trend last-line))))))))))]
    {:first-price (:p-new first-detail)
     :last-price (:p-new last-detail)
     :length (.size info)
     :trends trend-lines}))

(defn prn-analysis-info
  [info]
  (let [re (analysis-info info)]
    (str "\n"
         "length:\t" (:length re) "\n"
         "first price:\t" (:first-price re) "\n"
         "last price:\t" (:last-price re) "\n"
         "trends:\t" (reduce #(str %1 "\n" %2) "" (:trends re)) "\n")))
