(ns core
  (:require [clojure.spec.alpha :as s]
            [timelines.utils :as u]))

(defmacro defs [& args]
  (assert (even? (count args)))
  `(do ~@(for [[k v] (->> args (apply hash-map) seq)]
           `(def ~k ~v))))

(defmacro letmap [& args]
  (let [symbols (->> args (apply hash-map) keys)
        map-pairs (interleave (map keyword symbols) symbols)]
    `(let ~(into [] args)
       (hash-map ~@map-pairs))))

(defmacro defgraphics [& args]
  (let [{:keys [name params protocol-impls] :as conformed}
        (s/conform ::defgraphic-call args)]
    ;; (when (= :clojure.spec.alpha/invalid conformed)
    ;;   (s/explain ::defgraphic-call args)
    ;;   (throw (Exception. "Invalid arguments to defgraphic macro.")))
    ;; Process impls
    (let [protocol-impls (mapcat #(concat [(:protocol %)]
                                          (:impls %))
                                 protocol-impls)
          ;; protocol-impls (if-not (u/in? protocol-impls 'P-Samplable)
          ;;                  (concat protocol-impls ['P-Samplable default-samplable-impl])
          ;;                  protocol-impls)
          ]
      `(do
         (defrecord ~name ~params)
         (extend-type ~name
           ~@protocol-impls)))))

(defmacro defprotocol [& args]
  (let [{:keys [protocol-name]} (s/conform (s/cat :protocol-name simple-symbol?
                                                  :rest (s/* any?))
                                           args)
        qualified-name (u/resolve-sym-in-current-ns protocol-name)]
    (cons 'clojure.core/defprotocol args)))
