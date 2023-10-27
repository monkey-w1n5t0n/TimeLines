(ns timelines.protocols
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.core.reducers :as r]
            [timelines.utils :as u]
            [timelines.debug :refer :all]
            [clojure.walk :refer [postwalk prewalk]]
            [timelines.time :as t]
            [timelines.debug :refer [*dbg]]
            [timelines.macros :as m]))

;; General
(do
;;;; FUNCTOR
  ;; taken from https://github.com/jduey/Functors/blob/master/src/functors.clj
  (defprotocol P-Functor
    (fmap* [coll f]))

  ;; Convenience function because protocols dispatch on the first argument

  ;; TODO @correctness @performance test and profile these
  (extend-protocol P-Functor
    ;; TODO @performance there's got to be a better way for lists...
    ;; something like foldl so that it doesn't have to be reversed?
    clojure.lang.PersistentList
    (fmap* [coll f]
      (reverse
       (into '() (map f coll))))

    clojure.lang.PersistentVector
    (fmap* [coll f]
      (into [] (r/map f coll)))

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

  (s/fdef fmap
    :args any?
    :ret any?
    :fn #(= (type (second
                   (:args %)))
            (type (:ret %))))

  (comment
    (st/instrument `ranged-rand))

  (defn fmap [f obj]
    (cond
      (satisfies? P-Functor obj) (fmap* obj f)
      (record? obj) (u/map-record f obj)
      :default (throw (Exception. (str "ERROR FMAP: unknown type " (type obj))))))
  ;;
  )

;;;; UNBOXABLE
  ;; Not really used atm
(defprotocol P-Unboxable
  (unbox [this] "Unbox a datatype, e.g. to get the graph out of a Signal."))

(extend-protocol P-Unboxable
  Number
  (unbox [this] this)

  Long
  (unbox [this] this)

  Integer
  (unbox [this] this)

  clojure.lang.Ratio
  (unbox [this] this)

  Double
  (unbox [this] this)

  Float
  (unbox [this] this)

  String
  (unbox [this] this)

  clojure.lang.Keyword
  (unbox [this] this)

  clojure.lang.Symbol
  (unbox [this] this)

  clojure.lang.PersistentList
  (unbox [this]
    (apply list (map unbox this)))

  clojure.lang.PersistentVector
  (unbox [this]
    (apply vector (map unbox this))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Signal-related
(declare sample-at)

(do
  (defprotocol P-Bifunctor
    (premap [this f])
    (postmap [this f])
    (bimap [this f1 f2]))

  (defprotocol P-Samplable
    ;; TODO should this be time-varying too?
    (sample-at-impl [this t] "Evaluate a signal at a specific moment in time.")
    #_(signal-type [this] [this t] "The type of the signal. If only one argument is provided,
                                the type is assumed to be unchanging over time. If a time
                                argument is provided, the signal is sampled at that time
                                and the type of its value is returned."))

  (extend-protocol P-Samplable
    clojure.lang.Var
    (sample-at-impl [this t]
      (-> this var-get (sample-at-impl t)))

    clojure.lang.ArraySeq
    (sample-at-impl [this t]
      (into (empty this)
            (map #(sample-at-impl % t)
                 this)))

    clojure.lang.PersistentArrayMap
    (sample-at-impl [this t]
      (into {} (for [[k v] this]
                 [k (sample-at-impl v t)])))

    Number
    (sample-at-impl [this _] this)

    Long
    (sample-at-impl [this _] this)

    Integer
    (sample-at-impl [this _] this)

    clojure.lang.Ratio
    (sample-at-impl [this _] this)

    Double
    (sample-at-impl [this _] this)

    Float
    (sample-at-impl [this _] this)

    String
    (sample-at-impl [this _] this)

    clojure.lang.Keyword
    (sample-at-impl [this _] this)

    clojure.lang.Symbol
    (sample-at-impl [this _] this)

    clojure.lang.PersistentList
    ;; (sample-at-impl [this t]
    ;;   ;; (eval)
    ;;   ;; look at note re: into vs apply
    ;;   (println "list sample-at-implh")
    ;;   (into '()
    ;;         (map #(sample-at-impl % t) this)))
    (sample-at-impl [this t]
      (fmap #(sample-at-impl % t) this))

    clojure.lang.LazySeq
    (sample-at-impl [this t]
      (map #(sample-at-impl % t) this))

    clojure.lang.PersistentVector
    (sample-at-impl [this t]
      (into []
            (map #(sample-at % t) this)))))
;; SymbolicExpr
#_(do

    (defprotocol P-SymbolicExpr
      (->expr [this]))

    (templated-protocol-impl 'P-SymbolicExpr id-types '(->expr [this] this))

    (let [types (->> '[PersistentList PersistentVector]
                     (map #(u/symbol-prepend "clojure.lang." %)))]
      (templated-protocol-impl 'P-SymbolicExpr types '(->expr [this] (fmap ->expr this)))))

;; Drawing
(defprotocol P-Drawable
  "Draw a static object"
  (draw-impl [this] [this canvas]))

(s/def ::drawable #(satisfies? P-Drawable %))

(s/fdef draw
  :args ::drawable
  :ret any?)

(defn draw [x]
  (if (satisfies? P-Drawable x)
    (draw-impl x)))

(extend-protocol P-Drawable
  clojure.lang.PersistentVector
  (draw-impl [this] (doseq [x this]
                      (when x
                        (draw x))))

  clojure.lang.PersistentList
  (draw-impl [this] (doseq [x this]
                      (when x (draw x))))

  clojure.lang.PersistentHashMap
  (draw-impl [this] (doseq [x (vals this)]
                      (when x (draw x))))

  clojure.lang.PersistentArrayMap
  (draw-impl [this] (doseq [x (vals this)]
                      (when x
                        (draw x)))))

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

  #_(extend-protocol P-Range
      clojure.lang.PersistentVector
    ;; TODO @optimisation shouldn't have to travel the list twice
      (range [this] [(min this) (max this)])
      (range-at [this t]
        (let [sampled (mapv #(sample-at-impl % t) this)]
          [(min sampled) (max sampled)]))))

(defprotocol P-Skijable
  (->skija-impl [this]))

(defn ->skija [x]
  (->skija-impl x))
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

(s/def ::samplable #(satisfies? P-Samplable %))

(s/fdef sample-at
  :args (s/cat :sig ::samplable :time number?)
  ;; FIXME @correctness this should somehow test the return type?
  :ret any?)

(defn sample-at [x t]
  (when x
    (if (satisfies? P-Samplable x)
      (let [result (sample-at-impl x t)]
        result)
      (throw (Exception. (str "sample-at: " x ", " (type x)))))))

#_(comment
    (defn ->skija [x]
      (when @*dbg
        (println (str "->skija arg: " x)))
      (let [result (->skija-impl x)]
        (when @*dbg
          (println (str "->skija result: " result)))
        result)))

(s/fdef draw-at
  :args (s/cat :obj ::drawable :time number?)
  :ret any?)

(defn draw-at [obj t]
  (-> obj (sample-at t) draw))

(defn draw-now [x]
  (draw-at x (t/now)))

(defprotocol P-Dimensions
  (get-height-impl [this])
  (get-width-impl [this])
  (get-bounds-impl [this])
  (set-height-impl [this])
  (set-width-impl [this])
  (set-bounds-impl [this]))

(defn dbprint [x]
  (println x))

;; (s/fdef get-height
;;   :args ::dimensional
;;   :return ::samplable)

(s/def ::dimensional #(satisfies? P-Dimensions %))

(defn get-height [x]
  (get-height-impl x))

(defn get-width [x]
  (get-width-impl x))
