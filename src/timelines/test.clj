(ns timelines.test
  (:require  [clojure.test :as t]))

(defprotocol MyProtocol
  (my-function [this]))

(defn find-protocol [fn-name]
  (doseq [[_ v] (ns-publics *ns*)]
    (if (and (satisfies? clojure.lang.IObj v)
             (some? (find-ns (symbol (str (:name (meta v))))))
             (some? (get (meta v) :on-interface)))
      (let [proto (get (meta v) :on-interface)
            methods (seq (.getMethods proto))]
        (if (some #(= fn-name (str (.getName %))) methods)
          (println "Found function in protocol:" proto))))))

(-> (resolve 'my-function) meta)

(find-protocol "my-function")

P-
