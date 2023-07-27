(ns timelines.util.core
  (:require [timelines.editor.consts :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :as walk]
            ;; [timelines.parameters :refer :all]
            ;; [quil.middleware :as m]
            ;; [clojure.walk :as w]
            ;; [timelines.utils :refer :all]
            ))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn list-contains? [lst element]
  (some (set [element])
        lst))

(defn warn [msg]
  (println (str "Warning: " msg)))

(defn random-point-2D [width height]
  [(rand-int width) (rand-int height)])

(defn random-point-3D [width depth height]
  [(rand-int width) (rand-int depth) (rand-int height)])

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

;; NAMESPACE stuff, taken from (the now-deprecated) clojure-contrib, written by Stuart Sierra:
;; https://github.com/clojure/clojure-contrib/blob/a6a92b9b3d2bfd9a56e1e5e9cfba706d1aeeaae5/modules/with-ns/src/main/clojure/clojure/contrib/with_ns.clj#L20

(defn fn-arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))

(defn strip-symbol-ns-qualifiers [sym]
  (if (qualified-symbol? sym)
    (-> sym name symbol)
    sym))

(defn is-symbolic-fn? [form]
  (and
   (sequential? form)
   (= (-> form first strip-symbol-ns-qualifiers) 'fn)
   (instance? clojure.lang.PersistentVector (second form))))

(defn all? [pred col]
  (and (not (empty? col))
       (every? pred col)))

(defn sym->ns
  "Returns the ns that a symbol evaluates to in the current ns."
  [sym]
  (-> (resolve symbol)
      symbol
      namespace
      symbol
      find-ns))

(defn resolves-to-current-ns? [sym]
  (= *ns* (sym->ns sym)))

(defn replace-sym [tree from to]
  (walk/postwalk #(if (= % from) to %)
                 tree))

(defn strip-all-symbol-ns-qualifiers [coll]
  (if (coll? coll)
    (clojure.walk/postwalk strip-symbol-ns-qualifiers coll)
    (strip-symbol-ns-qualifiers coll)))

(defn record->map-constructor-fn [record]
  (let [record-name (-> record type .getName (str/split #"\.") last)]
    (->> record-name (str "map->") symbol resolve)))

  ;; TODO @completeness some way to tell whether f should be passed both the k and v or just the v?
  ;; having the k might be useful in determining what to do with the v
;; TODO @performance there has got to be a better way that doesn't create a billion intermediate objects...
#_(defn map-record [f record]
    (println (str "map-record, f: " f "\nrecord:" record))
    (let [constructor-fn (record->map-constructor-fn record)]
      (->> record
           (map (fn [[k v]]
                  [k (f v)]))
           flatten
           (apply hash-map)
           constructor-fn)))

(defn map-record [f record]
  (loop [acc record
         keys (keys record)]
    (if (empty? keys)
      acc
      (recur (update acc (first keys) f)
             (rest keys)))))
