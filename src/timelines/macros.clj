(ns timelines.macros
  (:require [timelines.utils :refer :all]
            [clojure.pprint :only [pprint]]))

;; TODO refactor
(defmacro defs [& args]
  (assert (even? (count args)))
  (let [pairs (loop [v args
                     acc []]
                (if
                 (empty? v) acc
                 (recur (drop 2 v)
                        (conj acc
                              `(def ~(first v) ~(second v))))))]
    `(do ~@pairs)))

(defmacro letmap [& args]
  (let [symbols (->> args (apply hash-map) keys)
        map-pairs (interleave (map keyword symbols) symbols)]
    `(let ~(into [] args)
       (hash-map ~@map-pairs))))

;; a thread macro that automatically handles fn literals by adding them to an otherwise empty list
;; (defmacro -> [&forms]
;;   ())

;; TODO write documentation about why or how you'd want to use these
;; THREADING MACROS
(defmacro <- [& body]
  `(-> ~@(reverse body)))

(defmacro <<- [& body]
  `(->> ~@(reverse body)))

(defmacro <-as [& body]
  (let [expr (-> body reverse first)
        name (-> body reverse second)]
    `(as-> ~expr ~name ~@(reverse body))))

;; TODO @properness this shouldn't be necessary if the idea of a main argument was implemented
;; TODO @extensibility instead of a hard-coded list of symbols, read from an atom?
(defmacro ->
  "Custom threading macro so that you can write stuff like `(-> t (slow 2))`
  even though `slow` expects the signal as the last argument, not the first"
  [& exprs]
  (let [special-fns (quote [fast slow])
        new-exprs (map
                   (fn [expr]
                     (cond
                       ;; Single symbol
                       (not (seq? expr))
                       expr

                       ;; Function call
                       (and (> (count expr) 2)
                            (in? special-fns (first expr)))
                       `((fn [x#]
                           (~@expr x#)))
                       :else expr))
                   exprs)]
    `(clojure.core/-> ~@new-exprs)))

;; NAMESPACE stuff
(defmacro with-ns
  "Evaluates body in another namespace.  ns is either a namespace
  object or a symbol.  This makes it possible to define functions in
  namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     ~@(map (fn [form] `(eval '~form)) body)))

(defmacro with-temp-ns
  "Evaluates body in an anonymous namespace, which is then immediately
  removed.  The temporary namespace will 'refer' clojure.core."
  [& body]
  `(try
     (create-ns 'sym#)
     (let [result# (with-ns 'sym#
                     (clojure.core/refer-clojure)
                     ~@body)]
       result#)
     (finally (remove-ns 'sym#))))

(defmacro argswap
  "Takes a form with 2 or more arguments and returns the
  same form with the first and last arguments swapped."
  [form]
  (let [[f & args] form
        first-arg (first args)
        last-arg (last args)
        middle-args (drop-last (drop 1 args))]
    `(~f ~last-arg ~@middle-args ~first-arg)))

;; TODO @dbg
(defmacro pprint-macroexpand-1 [expr]
  (let [expanded-expr (macroexpand-1 expr)
        cleaned-expr (clojure.walk/postwalk (fn [node]
                                              (if (symbol? node)
                                                (symbol (name node))
                                                node))
                                            expanded-expr)]
    `(clojure.pprint/pprint ~cleaned-expr)))

(defmacro pprint [expr]
  `(clojure.pprint/pprint (macroexpand-1 '~expr)))
