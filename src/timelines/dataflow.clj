(ns timelines.dataflow
  (:require [clojure.set :as set]))


(defonce *session
  (atom {:time -1
         :gui nil
         :synths nil
         :user-defined-symbols #{}}))

(defn s->known-symbols [s]
  (-> s :nodes keys))

(defn s->gui [s]
  (:gui s))

(defn s->synths [s]
  (:synths s))

(defn n->dependencies [n]
  (:dependencies n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn make-test-session []
  {:time 123
   :gui nil
   :synths nil
   :nodes
   {'foo {:expr 3
          :cache nil
          :dependents #{'bar}
          :dependencies #{}}
    'bar {:expr '(+ 1 foo)
          :cache nil
          :dependents #{}
          :dependencies #{'foo}}}})


(defn propagate-dirty [session symbol]
  )







(defn expr-to-update-form [exprs args]
  (list 'fn args )
  )


(defn symbol-intersection
  [expr known-symbols]
  (letfn [(walk [x]
            (cond
              (symbol? x) (if (contains? known-symbols x) #{x} #{})
              (seq? x) (apply clojure.set/union (map walk x))
              :else #{}))]
    (walk expr)))

(do
  (assert (= (symbol-intersection '(foo 1 bar c)
                                   '#{a b c d foo bar})
              '#{foo bar c}))

  )




(defn sort-symbols [symbol-set]
  (into [] (sort-by name symbol-set)))

(defn expr-dependencies [session expr]
  (sort-symbols
        (symbol-intersection expr (s->known-symbols session))))



(defn set-time [session new-time]
  (-> session
      (assoc :time new-time)
      (assoc :time-dirty? true)
      (propagate-dirty 't)))


(defn pull-node
  "Propagates changes for a node by recursively pulling its dependencies."
  [session node]
  (let [dependencies (n->dependencies node)]
    (loop [session session
           dependencies dependencies]
      (if (empty? dependencies)
        session
        (let [dep (first dependencies)
              expr (n->expr ) new-value])
        (recur (pull-node session (first dependencies))
               (rest dependencies))))))

(defn pull-roots [session]
  (-> session
      (pull-node (:root (s->gui session)))
      (pull-node (:root (s->synths session)))))

(defn timelines-update [session new-time]
  (->
   session
   (set-time new-time)
   pull-roots
   render-graphics
   update-synths))









(defrecord Node [id value dirty?])


(defn new-node [])


(defprotocol INode
  (get-value [this])
  (set-value! [this new-value])
  (add-dependent! [this dependent])
  (remove-dependent! [this dependent])
  (update-node! [this]))

(defrecord DataflowNode [id value dependencies dependents lambda]
  INode
  (get-value [_] @value)
  (set-value! [_ new-value] (reset! value new-value))
  (add-dependent! [_ dependent] (swap! dependents conj dependent))
  (remove-dependent! [_ dependent] (swap! dependents disj dependent))
  (update-node! [this]
    (let [dep-values (mapv get-value @dependencies)
          new-value (apply lambda dep-values)]
      (when (not= new-value @value)
        (set-value! this new-value)
        (doseq [dependent @dependents]
          (update-node! dependent))))))

(defn create-node
  ([id lambda]
   (create-node id lambda []))
  ([id lambda dependencies]
   (let [node (->DataflowNode id (atom nil) (atom (set dependencies)) (atom #{}) lambda)]
     (doseq [dep dependencies]
       (add-dependent! dep node))
     node)))

(def t (create-node :t (fn [] (System/currentTimeMillis))))

(defn propagate-network! []
  (let [new-time (System/currentTimeMillis)]
    (set-value! t new-time)
    (update-node! t)))

(defn create-dataflow-network []
  {:nodes {t t}
   :t t})

(defn add-node [network id lambda & dependencies]
  (let [node (create-node id lambda dependencies)]
    (update-in network [:nodes] assoc id node)))

(defn get-node [network id]
  (get-in network [:nodes id]))

(defn remove-node [network id]
  (let [node (get-node network id)
        dependencies @(:dependencies node)]
    (doseq [dep dependencies]
      (remove-dependent! dep node))
    (update network :nodes dissoc id)))

;; Example usage
(comment
  (def network (-> (create-dataflow-network)
                   (add-node :a (fn [t] (mod t 10)) t)
                   (add-node :b (fn [a] (* a 2)) :a)
                   (add-node :c (fn [a b] (+ a b)) :a :b)))

  (propagate-network!)

  (get-value (get-node network :a))
  (get-value (get-node network :b))
  (get-value (get-node network :c))

  (Thread/sleep 1000)
  (propagate-network!)

  (get-value (get-node network :a))
  (get-value (get-node network :b))
  (get-value (get-node network :c)))




;;;;;;;;;;;;
;;;;;;;;;;;; overtone
(use 'overtone.live)

(demo (sin-osc))





(defsynth my-simple-fm
  [freq 220]
  (let []))


(definst my-synth [freq 440 amp 0.5]
  (* (sin-osc freq)
     amp))


(def my-synth-instance (my-synth))

(ctl my-synth-instance :freq 200)
