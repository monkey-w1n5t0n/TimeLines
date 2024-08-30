(ns timelines.async
  (:require [clojure.core.async :as async :refer [>! <! go go-loop chan close! alts!]]))

(defn create-node [f in-channels out-channel]
  (go-loop []
    (let [inputs (atom [])
          all-received? (atom false)]
      (while (not @all-received?)
        (let [[val port] (async/alts! in-channels)]
          (if (nil? val)
            (swap! all-received? not)
            (swap! inputs conj val))))
      (when (= (count @inputs) (count in-channels))
        (let [result (apply f @inputs)]
          (>! out-channel result))))
    (recur)))

(defn create-source-node [value out-channel]
  (go
    (>! out-channel value)
    (close! out-channel)))

(defn create-sink-node [in-channel result-atom]
  (go-loop []
    (when-let [val (<! in-channel)]
      (reset! result-atom val)
      (recur))))

(defn connect-nodes [nodes]
  (doseq [[from to] (partition 2 1 nodes)]
    (when (and (:out from) (:in to))
      (async/pipe (:out from) (first (:in to))))))

(defn run-dataflow [graph]
  (let [nodes (mapv (fn [[node-key {:keys [f in out]}]]
                      (let [in-channels (if (seq in)
                                          (mapv (fn [_] (chan)) in)
                                          [(chan)])
                            out-channel (chan)]
                        (create-node f in-channels out-channel)
                        {:key node-key
                         :in in-channels
                         :out out-channel}))
                    (:nodes graph))
        node-map (into {} (map (juxt :key identity) nodes))]
    (connect-nodes (mapv node-map (:order graph)))
    node-map))

(defn example-dataflow []
  (let [graph {:nodes {:A {:f #(+ % 1) :in [] :out [:B :C]}
                       :B {:f #(* % 2) :in [:A] :out [:D]}
                       :C {:f #(- % 3) :in [:A] :out [:D]}
                       :D {:f + :in [:B :C] :out []}}
               :order [:A :B :C :D]}
        nodes (run-dataflow graph)
        result (atom nil)]

    (create-source-node 5 (-> nodes :A :in first))
    (create-sink-node (-> nodes :D :out) result)

    (Thread/sleep 1000) ; Wait for processing to complete
    @result))

(comment
  (example-dataflow) ; => 15
  )
