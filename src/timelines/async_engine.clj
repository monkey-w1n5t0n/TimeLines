(ns timelines.async-engine
  (:require [clojure.core.async :as a]))

(defonce graph (atom {}))

(defn create-node [name expr]
  {:name name
   :expr expr
   :chan (a/chan)
   :mult (atom nil)
   :value (atom nil)
   :dirty (atom true)
   :deps (atom #{})})

(defn update-deps! [graph node]
  (let [deps (atom #{})
        expr (:expr node)]
    (clojure.walk/postwalk
     (fn [form]
       (when (and (symbol? form) (contains? @graph form))
         (swap! deps conj form))
       form)
     expr)
    (reset! (:deps node) @deps)))

(defn insert-node!
  "Inserts a new node with an expression, e.g. name: c expr: '(+ a b)"
  [name expr]
  (let [node (create-node name expr)]
    (swap! graph assoc name node)
    (update-deps! graph node)
    (reset! (:mult node) (a/mult (:chan node)))
    (doseq [dep @(:deps node)]
      (let [dep-node (get @graph dep)
            dep-mult @(:mult dep-node)
            tap-chan (a/chan)]
        (a/tap dep-mult tap-chan)
        (a/go-loop []
          (when-let [val (a/<! tap-chan)]
            (reset! (:value dep-node) val)
            (a/>! (:chan node) val)
            (recur)))))))

(defn evaluate-node [graph node-name]
  (let [node (get @graph node-name)
        expr (:expr node)
        deps @(:deps node)]
    (if (empty? deps)
      (if (fn? expr) (expr) expr)
      (let [dep-values (map #(deref (:value (get @graph %))) deps)]
        (if (every? some? dep-values)
          (try
            (apply (eval `(fn [~@deps] ~expr)) dep-values)
            (catch Exception e
              (println "Error evaluating node" node-name ":" (.getMessage e))
              nil))
          (do
            (println "Not all dependencies are available for node" node-name)
            nil))))))

(defn compute-node! [graph node-name]
  (let [node (get @graph node-name)
        result (evaluate-node graph node-name)]
    (when result
      (reset! (:value node) result)
      (reset! (:dirty node) false)
      (a/put! (:chan node) result))
    result))

(defn mark-dirty! [graph node-name]
  (let [node (get @graph node-name)]
    (reset! (:dirty node) true)
    (doseq [downstream-node (vals @graph)]
      (when (contains? @(:deps downstream-node) node-name)
        (mark-dirty! graph (:name downstream-node))))))

(defn run-graph
  "Incrementally and asynchronously computes the output of the named node"
  [graph node-name]
  (let [node (get graph node-name)]
    (if @(:dirty node)
      (do
        (doseq [dep @(:deps node)]
          (run-graph graph dep))
        (compute-node! graph node-name))
      @(:value node))))

(defn initialize-graph! []
  (reset! graph {}))
