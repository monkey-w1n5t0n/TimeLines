(ns timelines.signal
  (:require [timelines.utils :as u]
            ;; [timelines.protocols :refer [P-Samplable P-Bifunctor
            ;;                              sample-at-impl postmap premap sample-at]]
            ;; [timelines.expr :as e]
            ;; [timelines.signal.api :refer :all]
            ;; [timelines.expr.core :as expr :refer :all]
            [clojure.pprint :refer [pprint]]
            ;; [timelines.signal :as signal]
            ))
;; TODO operators updating parameters such as :range, :min, :max etc


(def identity-signal :identity-signal)

(defn to-signal [expr]
  (let [env-comp (fn [env]
                   (let [final-form ()])
                   (fn [t]))])
  (eval expr))

((to-signal '(fn [x] x)) 5)

(defn sample-at [sig env ])



(import '[java.util.concurrent ConcurrentHashMap ExecutorService Executors])
(import '[java.util HashSet])

(deftype Node [^:volatile-mutable value
               ^clojure.lang.IFn compute-fn
               ^HashSet dependencies])

(def ^ConcurrentHashMap node-map (ConcurrentHashMap.))
(def ^ExecutorService executor (Executors/newWorkStealingPool))

(defn create-node [id compute-fn dependencies]
  (.put node-map id (->Node nil compute-fn (HashSet. dependencies))))

(defn update-node [^Node node]
  (let [new-value ((.compute-fn node))]
    (set! (.value node) new-value)
    (doseq [dep-id (.dependencies node)]
      (let [dep-node (.get node-map dep-id)]
        (.submit executor ^Runnable (fn [] (update-node dep-node)))))))

(defn update-t [new-t]
  (.put node-map 't new-t)
  (let [affected-nodes (.get node-map 't)]
    (doseq [node-id affected-nodes]
      (let [node (.get node-map node-id)]
        (.submit executor ^Runnable (fn [] (update-node node)))))))
