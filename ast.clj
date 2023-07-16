(ns timelines.ast)


(defmacro def-node-type [name fields]
  `(defrecord ~name ~fields))
