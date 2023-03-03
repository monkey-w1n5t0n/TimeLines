(ns timelines.util.core
  (:require [quil.core :as q]
            [timelines.editor.consts :refer :all]
            [clojure.pprint :refer [pprint]]
            ;; [timelines.parameters :refer :all]
            ;; [quil.middleware :as m]
            ;; [clojure.walk :as w]
            ;; [timelines.utils :refer :all]
            ))

(defn random-point [width height]
  [(rand-int width) (rand-int height)])

(defn with-indices
  "Takes a seq and returns a seq where each element is paired with its index"
  [seq]
  (map vector (-> seq count range) seq))

(defn pmap-indexed [f coll]
  (pmap f (iterate inc 0) coll))

(defn half [x]
  (/ x 2))

;; TODO use transients?
(defn update-map [m f]
  (reduce-kv (fn [m k v]
    (assoc m k (f v))) {} m))

(defn strip-symbol [sym]
  (if (qualified-symbol? sym)
    (-> sym name symbol)
    sym))

(defn strip-symbols [coll]
  (clojure.walk/postwalk strip-symbol coll))

;; NAMESPACE stuff, taken from (the now-deprecated) clojure-contrib, written by Stuart Sierra:
;; https://github.com/clojure/clojure-contrib/blob/a6a92b9b3d2bfd9a56e1e5e9cfba706d1aeeaae5/modules/with-ns/src/main/clojure/clojure/contrib/with_ns.clj#L20

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

(defn fn-arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))

(defn is-symbolic-fn? [form]
  (and
   (sequential? form)
   (= (first form) 'fn)
   (instance? clojure.lang.PersistentVector (second form))))
