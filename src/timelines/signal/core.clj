(ns timelines.signal.core
  (:require [timelines.util.core :as util]
            [timelines.protocols :refer :all]
            [timelines.expr.core :as expr]
            ;; [timelines.signal.api :refer :all]
            ;; [timelines.expr.core :as expr :refer :all]
            [clojure.pprint :refer [pprint]]
            [timelines.types :refer :all]
            ;; [timelines.signal :as signal]
            ))

;; TODO operators updating parameters such as :range, :min, :max etc

(declare apply-premap)
(declare apply-postmap)
(declare apply-bimap)

(comment
  ;; example API
  - (premap  t (+ 3))
  -> '(fn [t] (-> t
                 (+ 3)
                 ((fn [t0] t0))))

  - (postmap t (+ 3))
  -> '(fn [t] (-> t
                 ((fn [t0] t0))
                 (+ 3)))

  (premap (sine t) (/ 2)) ->
  -> (fn [t] (-> t
                (/ 2)
                ((fn [t1] (sine t1)))))

  (premap)
  )

;; TODO a signal should be const if e.g time is multiplied by 0
;; or replaced by a const
(defrecord Signal [expr const?]
  ;; SAMPLABLE
  P-Samplable
  ;; TODO @robustness make sure the time arg is correct type
  (sample-at [this time]
    (if const?
      (eval expr)
      ((eval expr) time)) )

  ;; FUNCTION
  clojure.lang.IFn
  (invoke [this time]
    (sample-at this time))

  P-Bifunctor
  (premap [this op]
    (if const?
      this
      (apply-premap this op)))
  (postmap [this op]
    (apply-postmap this op))

  (bimap [this pre-fn post-fn]
    (if const?
      (postmap this post-fn)
      (-> this
          (premap pre-fn)
          (postmap post-fn))))

  P-SymbolicExpression
  (->expr [this] expr)
  )


;; TODO abstract const expr checking into its own function
;; TODO test with postmap ops
(defn make-signal [expr]
  (if (instance? Signal expr)
    expr
    (->Signal expr
              (not (expr/sigfn? expr)))))



(make-signal '(fn [t] (+ t 2)))

;; TODO improve
(defmacro signal [expr]
  `(if (instance? Signal ~expr)
     (make-signal ~expr)
     (make-signal '~expr)))

(defmacro defsig [name expr]
  `(def ~name (signal ~expr)))

;; NOTE the most important signal
(defsig t (fn [time_0 ] time_0))

;; TODO @robustness more thorough checks
(defn time-arg [{:keys [expr const?] :as sig}]
  (assert (instance? Signal sig))
  (if const?
    nil
    (expr/sigfn->time-arg expr)))

;; TODO take into account expressions like (fn [t] 5), which should be considered const
;; TODO Simplify signal expression first
(defn const-sig? [x]
  (cond
    (instance? Signal x) (:const? x)
    :else true))

;; TODO take into account expressions like (fn [t] 5), which should be considered const
(defn var-sig? [x]
  (and (instance? Signal x)
       (not (:const? x))
       x))


(defn process-postmap-arg [arg new-time-arg-sym]
  (cond
    (var-sig? arg)
    (let [expr (:expr arg)]
      (expr/sigfn->replace-time-arg expr new-time-arg-sym))

    ;; TODO do this properly instead of separate cases for each type
    (instance? clojure.lang.PersistentVector arg)
    (into [] (map #(process-postmap-arg % new-time-arg-sym) arg))

    (instance? clojure.lang.PersistentList arg)
    (reverse (into '() (map #(process-postmap-arg % new-time-arg-sym) arg)))

    :else arg))


;; TEST THIS
;;;;;;;;;;;;;;;


;; TODO accept <op> to be a map with further info
;; e.g. arity, min-max etc
(defn apply-postmap [{:keys [sym source-ns] :as op} args]
  (if (util/all? const-sig? args)
    ;; TODO write this better
    ;; need to "unbox" all signals and leave everything else as-is
    (let [exprs (map ->expr args)]
      (make-signal `(~sym ~@exprs)))
    (let [qualified-sym (if source-ns
                          (ns-resolve source-ns sym)
                          sym)
          new-time-sym (gensym "time_")
          new-args (map #(process-postmap-arg % new-time-sym) args)
          ;; TODO produce this as persistent list directly?
          new-expr (->> `(fn [~new-time-sym] (~qualified-sym ~@new-args))
                        (into '())
                        reverse)]
      (make-signal new-expr))))

(defn apply-premap [sig time-fn]
  (if (const-sig? sig)
    sig
    (let [new-time-sym (gensym "time_")
          old-sigfn (:expr sig)
          new-expr (->> `(fn [~new-time-sym]
                           (apply ~old-sigfn
                                  [(apply ~time-fn [~new-time-sym]) ]))
                        (into '())
                        reverse)]
      (make-signal new-expr))))

;; TODO figure out how to define this for varargs
;; (defrecord Postmap-Op [op-sym docstr]
;;   ;; FUNCTION
;;   clojure.lang.IFn
;;   (applyTo [this args]
;;     (foo op-sym args)))
