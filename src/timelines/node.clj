(ns timelines.node
  )

(defrecord Id [val])
(defrecord Name [val])
(defrecord Body [val])
(defrecord Link [val])
(defrecord Link [val])

(def graph (atom {}))

(defrecord Node [id name body])

(defn references [x]
  x)

(defn referees [x]
  x)

(defn node [id name body]
  (->Node id name body (body->yper body) body->ypo body))

(node 1 2 3)
