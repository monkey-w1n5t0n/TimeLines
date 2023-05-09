(ns timelines.protocols
  (:require [clojure.core.reducers :as r]
            [timelines.util.core :as util]))

;; General
(defprotocol P-Unboxable
  (unbox [this] "Unbox a datatype, e.g. to get the graph out of a Signal."))

;; FUNCTOR
;; taken from https://github.com/jduey/Functors/blob/master/src/functors.clj
(defprotocol P-Functor
  (fmap [coll f]))

(extend-type clojure.lang.PersistentList
  P-Functor
  (fmap [coll f]
    (list* (into [] (r/map f coll)))))

(extend-type clojure.lang.PersistentVector
  P-Functor
  (fmap [coll f]
    (into [] (r/map f coll))))

(deftype reader-f [v rf f]
  clojure.lang.IFn
  (invoke [_ e]
    (if rf
      (f (rf e))
      v))

  P-Functor
  (fmap [rf f]
    (reader-f. nil rf f)))

(defn reader [v]
  (reader-f. v nil nil))

(defn read-e [rf f]
  (fn [e]
    (f e (rf e))))

(defprotocol P-Args-List
  (args-list [x]))

(defprotocol P-Symbolic-Apply
  (sym-apply [f-expr args-expr]))

;;;;;;;;;;;;;;;;;;;;;;;
;; Signal-related
(defprotocol P-Bifunctor
  (premap [this f])
  (postmap [this f])
  (bimap [this f1 f2]))

(defprotocol P-Sig-Func
  (sig-func [s]))

(defprotocol P-Samplable
  ;; TODO should this be time-varying too?
  (sample-at [this t] "Evaluate a signal at a specific moment in time.")
  (signal-type [this] [this t] "The type of the signal. If only one argument is provided,
                                the type is assumed to be unchanging over time. If a time
                                argument is provided, the signal is sampled at that time
                                and the type of its value is returned.")
  )

(extend-protocol P-Samplable
  Number
  (sample-at [this _] this)
  (signal-type
    ([this]   (class this))
    ([this _] (class this)))

  Long
  (sample-at [this _] this)
  (signal-type
    ([_] Long)
    ([_ _] Long))

  Integer
  (sample-at [this _] this)
  (signal-type
    ([_]   Integer)
    ([_ _] Integer))

  clojure.lang.Ratio
  (sample-at [this _] this)
  (signal-type
    ([_]   clojure.lang.Ratio)
    ([_ _] clojure.lang.Ratio))

  Double
  (sample-at [this _] this)
  (signal-type
    ([_]   Double)
    ([_ _] Double))

  Float
  (sample-at [this _] this)
  (signal-type
    ([_]   Float)
    ([_ _] Float))

  String
  (sample-at [this _] this)
  (signal-type
    ([_]   String)
    ([_ _] String))

  clojure.lang.Keyword
  (sample-at [this _] this)
  (signal-type
    ([_]   clojure.lang.Keyword)
    ([_ _] clojure.lang.Keyword))

  clojure.lang.Symbol
  (sample-at [this _] this)
  (signal-type
    ([_] clojure.lang.Symbol)
    ([_ _] clojure.lang.Symbol))

  clojure.lang.PersistentList
  ;; (sample-at [this t]
  ;;   ;; (eval)
  ;;   ;; look at note re: into vs apply
  ;;   (println "list sample-ath")
  ;;   (into '()
  ;;         (map #(sample-at % t) this)))
  (sample-at [this t]
    "hello")
  ;; TODO @robustness is this correct? probably not
  (signal-type
    ([this]
     (class (sample-at this 0)))
    ([this t]
     (class (sample-at this t))))

  clojure.lang.PersistentVector
  (sample-at [this t]
    (into []
          (map #(sample-at % t) this)))
  ;; TODO @robustness is this correct?
  (signal-type
    ([this]
     (class (sample-at this 0)))
    ([this t]
     (class (sample-at this t))))

  ;; clojure.lang.Fn
  ;; (sample-at [this t]
  ;;   (assert (= 1 (util/fn-arg-count this)))
  ;;   (this t))
  )

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
    (apply vector (map unbox this)))
  )


(defprotocol P-SymbolicExpression
  (->expr [this]))

(extend-protocol P-SymbolicExpression
  Number
  (->expr [this] this)

  Long
  (->expr [this] this)

  Integer
  (->expr [this] this)

  clojure.lang.Ratio
  (->expr [this] this)

  Double
  (->expr [this] this)

  Float
  (->expr [this] this)

  String
  (->expr [this] this)

  clojure.lang.Keyword
  (->expr [this] this)

  clojure.lang.Symbol
  (->expr [this] this)

  clojure.lang.PersistentList
  (->expr [this]
    (apply list (map ->expr this)))

  clojure.lang.PersistentVector
  (->expr [this]
    (apply vector (map ->expr this)))
  )


;; PROTOCOLS
(defprotocol P-Drawable
  "Draw a static object"
  (draw [this] "Draw a (non-signal) graphics object."))

(defprotocol P-Samplable+Drawable
  (draw-at [this t] "Draw a signal graphics object."))
