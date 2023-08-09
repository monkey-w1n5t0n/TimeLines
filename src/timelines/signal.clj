(ns timelines.signal
  (:require [timelines.utils :as u]
            [timelines.protocols :refer [P-Samplable P-Bifunctor
                                         sample-at-impl postmap premap sample-at]]
            [timelines.expr :as e]
            ;; [timelines.signal.api :refer :all]
            ;; [timelines.expr.core :as expr :refer :all]
            [clojure.pprint :refer [pprint]]
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

  (premap))

;; TODO a signal should be const if e.g time is multiplied by 0
;; or replaced by a const
(defrecord Signal [expr const?]
  ;; SAMPLABLE
  P-Samplable
  ;; TODO @robustness make sure the time arg is correct type
  ;; TODO @correctness the expr may actually be an fn of no arguments,
  ;; in which case it should be evalled and then called
  (sample-at-impl [this time]
    (if const?
      (eval expr)
      ((eval expr) time)))

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
          (postmap post-fn)))))

;; TODO abstract const expr checking into its own function
;; TODO test with postmap ops
(defn make-signal [e]
  (if (instance? Signal e)
    e
    (->Signal e (e/const? e))))

(defn ->expr [x]
  (condp instance? x
    Signal (:expr x)
    clojure.lang.PersistentList (->> x (map #(->expr %)) (into '()) reverse)
    clojure.lang.PersistentVector (transduce (map ->expr) conj x)
    x))

(comment
  (make-signal '(fn [t] (+ t 2))))

;; TODO improve
(defmacro signal [expr]
  (assert (= (first expr) 'fn))
  `(if (instance? Signal ~expr)
     (make-signal ~expr)
     (make-signal '~expr)))

(defmacro defsig [name expr]
  `(def ~name (signal ~expr)))

;; NOTE the most important signal

;; TODO @robustness more thorough checks
(defn time-arg [{:keys [expr const?] :as sig}]
  (assert (instance? Signal sig))
  (if const?
    nil
    (e/time-arg expr)))

;; TODO take into account expressions like (fn [t] 5), which should be considered const
;; TODO Simplify signal expression first
(defn const-sig? [x]
  (cond
    (instance? Signal x) (:const? x)
    :else true))

;; TODO
(defn const? [x]
  (cond
    (instance? Signal x) (const-sig? x)
    (associative? x) (every? const? (vals x))
    :else true))

(comment

  (loop [[k v]]
    (println (str k " " v))))

(every? pos? (vals {:x -1 :y 2}))

;; TODO @completeness take into account expressions like (fn [t] 5), which should be considered const
(defn var-sig? [x]
  (and x
       (instance? Signal x)
       (not (:const? x))))

;; TEST THIS
;;;;;;;;;;;;;;;

;; TODO accept <op> to be a map with further info
;; e.g. arity, min-max etc
#_(defn apply-postmap [op ns args]
    (let [qualified-sym (ns-resolve (or source-ns *ns*) sym)]
      (if (u/all? const-sig? args)
      ;; TODO write this better
      ;; need to "unbox" all signals and leave everything else as-is
        (let [exprs (map ->expr args)]
          (make-signal `(~qualified-sym ~@exprs)))
        (let [new-time-sym (gensym "time_")
              new-args (map #(process-postmap-arg % new-time-sym) args)
            ;; TODO produce this as persistent list directly?
              new-expr (->> `(fn [~new-time-sym] (~qualified-sym ~@new-args))
                            (into '())
                            reverse)]
          (make-signal new-expr)))))

#_(defn apply-postmap [op args]
    (let [op (if (simple-symbol? op)
               (ns-resolve *ns* op)
               op)]
        ;; All const sigs
      (if (u/all? const-sig? args)
        (let [exprs (map ->expr args)]
          (make-signal `(~op ~@exprs)))
          ;; At least one variable sig
        (let [time-arg 't
              arg-exprs (mapv ->expr args)
              processed-arg-exprs (mapv #(if (e/sigfn? %)
                                           (list '-> time-arg (list %))
                                           %)
                                        arg-exprs)
                ;; TODO produce this as persistent list directly?
              new-expr (list 'clojure.core/fn [time-arg]
                             (cons op processed-arg-exprs))]
          (make-signal new-expr)))))

(defn apply-postmap [op args]
  (make-signal
   (e/apply-postmap op (map ->expr args))))

;; TODO @correctness f should probably be applied to time even if the signal is constant
;; (while maintaining the :const? attribute)
;; so that, in the future, if we want to replace a signal's body but keep
;; all its time transformations we have the option to
(defn apply-premap [f sig]
  (make-signal (e/sigfn-premap (->expr sig) (->expr f))))

;; TODO figure out how to define this for varargs
;; (defrecord Postmap-Op [op-sym docstr]
;;   ;; FUNCTION
;;   clojure.lang.IFn
;;   (applyTo [this args]
;;     (foo op-sym args)))
