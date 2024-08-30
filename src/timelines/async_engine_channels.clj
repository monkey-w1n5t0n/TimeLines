(ns timelines.async-engine-channels
  (:require [clojure.core.async :as a]))

;;; Graph Structure
;;; ===============
;;;
;;;          +--------+
;;;          |  Graph |
;;;          +--------+
;;;              |
;;;              | contains
;;;              v
;;;          +--------+
;;;          |  Node  |
;;;          +--------+
;;;          |  name  |
;;;          |  expr  |
;;;          |  chan  |
;;;          |  mult  |
;;;          |  value |
;;;          |  dirty |
;;;          |  deps  |
;;;          +--------+
;;;
;;; The graph is an atom containing a map of node names to node structures.
;;; Each node represents a computation in the graph.

(defonce graph (atom {}))

;;; Node Creation
;;; =============

(defn create-node
  "Creates a new node structure with the given name and expression.

   Inputs:
   - name: A symbol representing the node's name
   - expr: The expression to be evaluated for this node

   Output:
   - A map representing the node structure

   State:
   - Does not modify any state

   Note: This function only creates the structure, it doesn't add it to the graph."
  [name expr]
  {:name name              ; The name of the node (symbol)
   :expr expr              ; The expression to be evaluated (can be a value or a function)
   :chan (a/chan)          ; Channel for communicating node updates
   :mult (atom nil)        ; Mult for broadcasting updates to multiple receivers
   :value (atom nil)       ; Current value of the node
   :dirty (atom true)      ; Flag indicating if the node needs recomputation
   :deps (atom #{})})      ; Set of dependencies (other nodes this node depends on)

;;; Dependency Management
;;; =====================

(defn update-deps!
  "Updates the dependencies of a node based on its expression.

   Inputs:
   - graph: The current graph state
   - node: The node to update dependencies for

   Output:
   - None (side-effecting function)

   State:
   - Modifies the :deps atom of the given node

   Process:
   1. Create a new atom to collect dependencies
   2. Walk through the node's expression
   3. For each symbol in the expression, check if it's a node in the graph
   4. If it is, add it to the dependencies
   5. Update the node's :deps atom with the collected dependencies"
  [graph node]
  (let [deps (atom #{})
        expr (:expr node)]
    (clojure.walk/postwalk
     (fn [form]
       (when (and (symbol? form) (contains? @graph form))
         (swap! deps conj form))
       form)
     expr)
    (reset! (:deps node) @deps)))

;;; Node Insertion
;;; ==============

(defn insert-node!
  "Inserts a new node into the graph and sets up its dependencies.

   Inputs:
   - name: The name for the new node
   - expr: The expression for the new node

   Output:
   - None (side-effecting function)

   State:
   - Modifies the global graph atom
   - Creates new channels and mults

   Process:
   1. Create a new node
   2. Add the node to the graph
   3. Update the node's dependencies
   4. Create a mult for the node's channel
   5. For each dependency:
     a. Create a tap on the dependency's mult
     b. Start a go-loop to propagate values from the dependency to this node

   Flow diagram:

     insert-node!
         |
         v
    create-node
         |
         v
    add to graph
         |
         v
    update-deps!
         |
         v
    create mult
         |
         v
    for each dep:
      |
      +-> tap dep's mult
      |
      +-> start go-loop
          for propagation
  "
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

;;; Node Evaluation
;;; ===============

(defn evaluate-node
  "Asynchronously evaluates a node's expression.

   Inputs:
   - graph: The current graph state
   - node-name: The name of the node to evaluate

   Output:
   - A channel that will contain the result of the evaluation

   State:
   - Does not directly modify state, but may trigger state changes in other functions

   Process:
   1. Retrieve the node from the graph
   2. If the node has no dependencies, evaluate its expression directly
   3. If the node has dependencies:
     a. Create taps for all dependency channels
     b. Collect values from all dependencies
     c. Apply the node's expression to the dependency values
   4. Return the result (or nil if there's an error)

   Error handling:
   - Catches and logs any exceptions during evaluation
   - Returns nil if not all dependencies are available

   Flow diagram:

    evaluate-node
         |
         v
    node has deps?
      |
      +-> No: evaluate expression directly
      |
      +-> Yes: collect dep values
               |
               v
          apply expression
               |
               v
          return result
  "
  [graph node-name]
  (a/go
    (let [node (get @graph node-name)
          expr (:expr node)
          deps @(:deps node)]
      (if (empty? deps)
        (if (fn? expr) (expr) expr)
        (let [dep-chans (map #(a/tap @(:mult (get @graph %)) (a/chan)) deps)
              dep-values (a/<! (a/into [] (a/merge dep-chans)))]
          (if (every? some? dep-values)
            (try
              (apply (eval `(fn [~@deps] ~expr)) dep-values)
              (catch Exception e
                (println "Error evaluating node" node-name ":" (.getMessage e))
                nil))
            (do
              (println "Not all dependencies are available for node" node-name)
              nil)))))))

;;; Node Computation
;;; ================

(defn compute-node!
  "Computes the value of a node and updates its state.

   Inputs:
   - graph: The current graph state
   - node-name: The name of the node to compute

   Output:
   - A channel that will contain the computed result

   State:
   - Updates the node's :value atom
   - Sets the node's :dirty atom to false
   - Puts the new value onto the node's channel

   Process:
   1. Retrieve the node from the graph
   2. Evaluate the node using evaluate-node
   3. If a result is obtained:
     a. Update the node's value
     b. Mark the node as not dirty
     c. Put the new value onto the node's channel
   4. Return the result

   Flow diagram:

    compute-node!
         |
         v
    evaluate-node
         |
         v
    result obtained?
      |
      +-> Yes: update value
      |        mark not dirty
      |        put on channel
      |
      +-> No: do nothing
         |
         v
    return result
  "
  [graph node-name]
  (a/go
    (let [node (get @graph node-name)
          result (a/<! (evaluate-node graph node-name))]
      (when result
        (reset! (:value node) result)
        (reset! (:dirty node) false)
        (a/>! (:chan node) result))
      result)))

;;; Dependency Tracking
;;; ===================

(defn mark-dirty!
  "Marks a node and all its downstream dependencies as dirty.

   Inputs:
   - graph: The current graph state
   - node-name: The name of the node to mark as dirty

   Output:
   - A channel that completes when all marking is done

   State:
   - Sets the :dirty atom to true for the node and its downstream dependencies

   Process:
   1. Mark the current node as dirty
   2. For each node in the graph:
     a. If it depends on the current node, recursively mark it as dirty

   Flow diagram:

    mark-dirty!
         |
         v
    mark node dirty
         |
         v
    for each node in graph:
      |
      +-> depends on current?
          |
          +-> Yes: recursively mark-dirty!
          |
          +-> No: skip
  "
  [graph node-name]
  (a/go
    (let [node (get @graph node-name)]
      (reset! (:dirty node) true)
      (doseq [downstream-node (vals @graph)]
        (when (contains? @(:deps downstream-node) node-name)
          (a/<! (mark-dirty! graph (:name downstream-node))))))))

;;; Graph Execution
;;; ===============

(defn run-graph
  "Incrementally and asynchronously computes the output of the named node.

   Inputs:
   - graph: The current graph state
   - node-name: The name of the node to compute

   Output:
   - A channel that will contain the computed value of the node

   State:
   - May trigger computations and state updates in other nodes

   Process:
   1. Check if the node is dirty
   2. If dirty:
     a. Recursively run-graph on all dependencies
     b. Compute the node
   3. If not dirty, return the current value

   Flow diagram:

    run-graph
         |
         v
    node is dirty?
      |
      +-> Yes: for each dep:
      |         |
      |         +-> run-graph
      |         v
      |        compute-node!
      |
      +-> No: return current value
  "
  [graph node-name]
  (a/go
    (let [node (get graph node-name)]
      (if @(:dirty node)
        (do
          (doseq [dep @(:deps node)]
            (a/<! (run-graph graph dep)))
          (a/<! (compute-node! graph node-name)))
        @(:value node)))))

;;; Graph Initialization
;;; ====================

(defn initialize-graph!
  "Resets the graph to an empty state.

   Inputs:
   - None

   Output:
   - None

   State:
   - Resets the global graph atom to an empty map

   Note: This function should be called before building a new graph from scratch."
  []
  (reset! graph {}))

;;; Overall System Flow
;;; ===================
;;;
;;;   1. initialize-graph!
;;;   2. insert-node! (for each node in the graph)
;;;   3. run-graph (to compute specific nodes)
;;;   4. mark-dirty! (when inputs change)
;;;   5. run-graph (to recompute affected nodes)
;;;
;;; This system allows for efficient, incremental, and asynchronous
;;; computation of interdependent values in a graph structure.
