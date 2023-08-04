(ns timelines.protocols
  (:require [clojure.core.reducers :as r]
            [timelines.utils :as u]
            [clojure.walk :refer [postwalk prewalk]]
            [clojure.spec.alpha :as s]
            [timelines.macros :as m]))

(def id-types '[Number Long Integer Double Float clojure.lang.Ratio
                String clojure.lang.Keyword clojure.lang.Symbol])

(defn templated-protocol-impl [protocol types & bodies]
  (doseq [t types]
    (let [expr (concat (list 'extend-protocol protocol t)
                       bodies)]
      (println expr)
      (eval expr))))

;; General
(do
;;;; FUNCTOR
  ;; taken from https://github.com/jduey/Functors/blob/master/src/functors.clj
  (defprotocol P-Functor
    (fmap* [coll f]))

  ;; Convenience function because protocols dispatch on the first argument
  (defn fmap [f obj]
    (fmap* obj f))

  (let [types (->> '[PersistentList PersistentVector ArraySeq]
                   (map #(u/symbol-prepend "clojure.lang." %)))]
    (templated-protocol-impl 'P-Functor
                             types
                             '(fmap* [this f]
                                     (into (empty this) (r/map f this)))))

  ;; TODO @correctness @performance test and profile these
  (extend-protocol P-Functor
    clojure.lang.PersistentArrayMap
    (fmap* [coll f]
      (reduce (fn [acc [k v]]
                (assoc acc k (f v)))
              (empty coll)
              coll))

    clojure.lang.PersistentHashMap
    (fmap* [coll f]
      (reduce (fn [acc [k v]]
                (assoc acc k (f v)))
              (empty coll)
              coll)))

;;;; UNBOXABLE
  ;; Not really used atm
  (defprotocol P-Unboxable
    (unbox [this] "Unbox a datatype, e.g. to get the graph out of a Signal."))

  (templated-protocol-impl 'P-Unboxable id-types '(unbox [this] this))

  ;; TODO @performance would it be faster to use Specter for this and similar?

  ;; ID types
  (let [types (->> '[PersistentList PersistentVector PersistentArrayMap PersistentHashMap]
                   (map #(u/symbol-prepend "clojure.lang." %)))]
    (templated-protocol-impl 'P-Unboxable
                             types
                             '(unbox [this] (fmap unbox this)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Signal-related
(do
  (defprotocol P-Bifunctor
    (premap [this f])
    (postmap [this f])
    (bimap [this f1 f2]))

  (defprotocol P-Samplable
    ;; TODO should this be time-varying too?
    (sample-at [this t] "Evaluate a signal at a specific moment in time.")
    #_(signal-type [this] [this t] "The type of the signal. If only one argument is provided,
                                the type is assumed to be unchanging over time. If a time
                                argument is provided, the signal is sampled at that time
                                and the type of its value is returned."))
  ;; ID types
  (templated-protocol-impl 'P-Samplable id-types
                           '(sample-at [this _] this))

  (let [types (->> '[ArraySeq PersistentList PersistentVector PersistentArrayMap PersistentHashMap]
                   (map #(u/symbol-prepend "clojure.lang." %)))]
    (templated-protocol-impl 'P-Samplable types '(sample-at [this t] (fmap #(sample-at % t) this))))

  (extend-protocol P-Samplable
    clojure.lang.Var
    (sample-at [this t]
      (-> this var-get (sample-at t)))
    ;; Does it even make sense to sample a function (i.e. getting back another function)?
    ;; clojure.lang.Fn
    ;; (sample-at [this t]
    ;;   (assert (= 1 (u/fn-arg-count this)))
    ;;   (this t))
    ))

;; SymbolicExpr
(do

  (defprotocol P-SymbolicExpr
    (->expr [this]))

  (templated-protocol-impl 'P-SymbolicExpr id-types '(->expr [this] this))

  (let [types (->> '[PersistentList PersistentVector]
                   (map #(u/symbol-prepend "clojure.lang." %)))]
    (templated-protocol-impl 'P-SymbolicExpr types '(->expr [this] (fmap ->expr this)))))

;; Drawing
(defprotocol P-Drawable
  "Draw a static object"
  #dbg(draw [this] [this canvas]))

#_(let [types (->> '[PersistentList PersistentVector]
                   (map #(u/symbol-prepend "clojure.lang." %)))]
    (templated-protocol-impl 'P-Drawable types
                             '(draw [this] (draw this @timelines.globals/*main-canvas))
                             '(draw [this canvas] (doseq [x this]
                                                    (when x (draw x canvas))))))

(extend-protocol P-Drawable
  clojure.lang.PersistentVector
  (draw [this] (draw this @timelines.globals/*main-canvas))
  (draw [this canvas]
    (doseq [x this] (when x
                      (draw x canvas)))))

(let [types (->> '[PersistentArrayMap PersistentHashMap]
                 (map #(u/symbol-prepend "clojure.lang." %)))]
  (templated-protocol-impl 'P-Drawable types
                           '(draw [this] (draw this @timelines.globals/*main-canvas))
                           '(draw [this canvas] (doseq [x (vals this)]
                                                  (when x (draw x canvas))))))

;; (defprotocol P-Samplable+Drawable
;;   (draw-at [this t] "Draw a signal graphics object."))

;; Range
(do
  (s/def ::range (s/and (s/cat :start number? :end number?)
                        #(<= (:start %) (:end %))))

  (defprotocol P-Range
    (range [this])
    (range-at [this t])
    (range-at-segment [this {:keys [start end]}]))

  (extend-protocol P-Range
    clojure.lang.PersistentVector
    ;; TODO @optimisation shouldn't have to travel the list twice
    (range [this] [(min this) (max this)])
    (range-at [this t]
      (let [sampled (mapv #(sample-at % t) this)]
        [(min sampled) (max sampled)]))))

(defprotocol P-Skijable
  (->skija [this]))

;; Unused
(do
  (deftype reader-f [v rf f]
    clojure.lang.IFn
    (invoke [_ e]
      (if rf
        (f (rf e))
        v))

    P-Functor
    (fmap* [rf f]
      (reader-f. nil rf f)))

  (defn reader [v]
    (reader-f. v nil nil))

  (defn read-e [rf f]
    (fn [e]
      (f e (rf e)))))

(defn draw-at [obj t]
  (-> obj (sample-at t) draw))
