(ns timelines.signal.core
  (:require [timelines.util.core :as util]
            [timelines.protocols :refer :all]
            ;; [timelines.expr.core :as expr :refer :all]
            [clojure.pprint :refer [pprint]]
            [timelines.types :refer :all]
            ;; [timelines.signal :as signal]
            ))


;; TODO a signal should be non-const if e.g time is multiplied by 0
;; or replaced by a const
(defrecord Signal [expr const?]
  ;; SAMPLABLE
  P-Samplable
  ;; TODO @robustness make sure the time arg is correct type
  (sample-at [this time]
    (if const?
      (eval expr)
      ((eval expr) time)))

  ;; FUNCTION
  clojure.lang.IFn
  (invoke [this time]
    (sample-at this time))


  P-Bifunctor
  (fmap-in [this in-fn]
    (if const?
      this
      (Signal. (util/strip-symbols
                `(fn [time_input#]
                   (-> time_input#
                       ~in-fn
                       ~expr)))
               const?)))
  (fmap-out [this out-fn]
    (if const?
      (Signal.
       (util/strip-symbols
        `(-> ~expr out-fn))
       const?)
      (Signal.
       (util/strip-symbols
        `(fn [time_input#]
           (-> time_input# ~expr ~out-fn)))
       const?)))

  (bimap [this in-fn out-fn]
    (if const?
      (fmap-out this out-fn)
      (-> this
          (fmap-in in-fn)
          (fmap-out out-fn))))
  )

;; TODO abstract const expr checking into its own function
;; TODO take into account expressions like (fn [t] 5), which should be considered const
(defmacro signal [body]
  (let [const? (cond
                 (and (instance? clojure.lang.PersistentList body)
                      (= (first body) 'fn))
                 false

                 (instance? Signal body) (:const? body)

                 :else true)]
    `(Signal. '~(util/strip-symbols body) ~const?)))

(defmacro defsig [name expr]
  (if (instance? Signal expr)
    `(def ~name ~expr)
    `(def ~name (signal ~expr))))

(comment
  (signal (fn [t] 5))
  (macroexpand-1 '(defsig t (fn [t] t))))

(defsig t (fn [global_time (println t)] global_time))

(every? #(not (and (instance? Signal %) (not (:const? %)))) [(signal (fn [t] t)) 4])



(defn fn-expr? [expr]
  (and (instance? clojure.lang.PersistentList expr)
       (= 'fn (first expr))))

(defn signal-time-arg-symbol [{:keys [expr]}]
  (if (fn-expr? expr)
    (-> expr second first)
    nil))


(comment


  (+ (fn [t] t) 1) -> (fn [t] (+ t 1))

  (slow 2 (fn [t] (+ 1 t))) -> (fn [t] (-> t (/ 2) (fn [t] (+ 1 t))))


  )





;; (comment


;;   (declare const-sig?)

;;   ;; NOTE @semantics
;;   ;; maybe `sf` shouldn't automatically be the result of `(eval expr)`?
;;   ;; for example what about a signal that should always just return the list
;;   ;; `(fn [x] x)`?
;;   ;; TODO @completeness add more metadata, e.g. range
;;   (defrecord Signal [expr sf type const?]
;;     clojure.lang.IFn
;;     (invoke [this time]
;;       (sample-at this time))
;;     (applyTo [this args]
;;       (try
;;         (apply sf args)
;;         (catch Exception e
;;           (println (str "Error: can only apply a Signal if its
;;                        Signal Function is an actual function, now it's: "
;;                         (type sf) ".\n->" (.getMessage e))))))

;;     P-Samplable
;;     ;; TODO @robustness make sure the time arg is correct type
;;     (sample-at [_ time]
;;       (if const?
;;         sf
;;         (sample-at sf time)))

;;     (signal-type [this time]
;;       type)

;;     ;; FIXME @correctness check on the combination of
;;     ;; const and non-const signals and functions
;;     ;; P-Sig-Functor
;;     ;; (fmap-input [this sym-f]
;;     ;;   (if const?
;;     ;;     ;; Const sig
;;     ;;     this
;;     ;;     ;; Non-const sig
;;     ;;     (let [new-expr (fmap-input expr sym-f)]
;;     ;;       (-> this
;;     ;;           (assoc :expr new-expr)
;;     ;;           (assoc :sf (eval new-expr))))))

;;     ;; (fmap-output [this sym-f]
;;     ;;   (let [new-expr (fmap-output expr sym-f)]
;;     ;;     (-> this
;;     ;;         (assoc :expr new-expr)
;;     ;;         (assoc :sf (eval new-expr)))))

;;     P-Bifunctor
;;     (bimap [this f1 f2]
;;       (-> this (fmap-input f1) (fmap-output f2)))
;;     )


;;   (comment
;;     (-> (Signal. '(fn [t] (+ t 1))
;;                  (fn [t] (+ t 1))
;;                  nil
;;                  false)
;;         (fmap-output '(+ 1)))
;;     )

;;   (comment
;;     (pprint
;;      (symbolic-apply-to-input (:expr t) '(fn [x] (+ 1 x))))

;;     (fmap-input t '(fn [x] (+ x 1)))

;;     (pprint
;;      (fmap-output t '(fn [x] (+ 1 x))))

;;     (fmap-output (make-signal 3) '(fn [x] (inc x)))

;;     (symbolic-apply-to-output 3 '(fn [x] (inc x))))


;;   (comment
;;     (sample-at 1 123.78)
;;     (sample-at (int 4) 123.78)
;;     (sample-at 1.1 123.78)
;;     (sample-at "hello" 123.78)
;;     (sample-at :key 123.78)
;;     (sample-at 'sym 123.78)
;;     ;; NOTE @semantics hmm this returns a fn object, maybe it shouldn't eval?
;;     (sample-at '(fn [i] i) 123.78)
;;     (sample-at 1 123.78))


;; ;;   ;; HELPERS
;; ;;   (defn const-expr? [expr]
;; ;;     (not (util/is-symbolic-fn? expr)))


;; ;;   (defn const-sig? [sig]
;; ;;     (if (instance? Signal sig)
;; ;;       (:const? sig)
;; ;;       (isa? (type sig) ::Static-Signal)))

;; ;;   (defn all-const-sigs? [sigs]
;; ;;     (->> sigs
;; ;;          (map (comp not const-sig?))
;; ;;          (some identity)
;; ;;          not))


;; ;;   (comment
;; ;;     (all-const-sig? '(fn [x] x) )
;; ;;     (all-const-sig? (+ 1 2)))

;; ;;   (comment
;; ;;     (all-const-sig? [(signal 1) (signal "hello")])
;; ;;     (all-const-sig? [(signal 'bob) (signal (fn [x] x))]))

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;   ;; Main Signal Constructor
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;   (defn make-signal [expr]
;; ;;     (let [symbolic-fn? (util/is-symbolic-fn? expr)
;; ;;           const? (not symbolic-fn?)
;; ;;           sf (eval expr)
;; ;;           type (if const?
;; ;;                  (type sf)
;; ;;                  :type/TODO)
;; ;;           expr (if const?
;; ;;                  (expr/make-Const-Expr expr type)
;; ;;                  (expr/make-Time-Expr  expr type))]
;; ;;       (Signal. expr sf type const?)))

;; ;;   (defmacro signal
;; ;;     ([expr]
;; ;;      `(make-signal '~expr)))

;; ;;   (def time-identity-expr '(fn [t] t))

;; ;;   (defn Signal->expr [s]
;; ;;     (:expr s))

;; ;;   (defn is-time-identity-expr? [expr]
;; ;;     (= time-identity-expr expr))

;; ;;   (defn is-time-identity-sig? [s]
;; ;;     (and (instance? Signal s)
;; ;;          (is-time-identity-expr? (Signal->expr s))))

;; ;;   ;; The workhorse signal
;; ;;   (def t (signal (fn [t] t)))

;; ;;   ;; Modifiers
;; ;;   (defn scale-time [amt s]
;; ;;     (fmap-input
;; ;;      s
;; ;;      (util/strip-symbols
;; ;;       `(fn [x#] (* ~amt x#)))))

;; ;;   (defn pprint-signal [s & file]
;; ;;     (let [file (or file() nil)
;; ;;           string (pprint s)]
;; ;;       (if file
;; ;;         (spit file string))))


;; ;;   ;; (pprint-signal )

;; ;;   (comment

;; ;;     (pprint (scale-time 3 (make-signal '(fn [x] (+ x 1))))))

;; ;;   (fmap-input (make-signal '(fn [x] (+ x 1)))
;; ;;               '(fn [b] (/ b 2)))

;; ;;   (defn fast [amt s]
;; ;;     (scale-time amt s))

;; ;;   (defn slow [amt s]
;; ;;     (scale-time (/ amt) s))

;; ;;   (defn ->expr [s]
;; ;;     (if (instance? Signal s)
;; ;;       (Signal->expr s)
;; ;;       s))

;; ;;   (instance? Signal t)


;; ;;   ;; (defmacro make-sig-op [sym]
;; ;;   ;;   `(fn [& args#]
;; ;;   ;;      (signal (cons '~sym (map graph args#)))))

;; ;;   ;; TODO add support for specific arities other than variadic?
;; ;;   ;; TODO @completeness do better checking for whether the new
;; ;;   ;; signal will be const or not
;; ;;   (defmacro defop [sym]
;; ;;     `(defn ~(util/strip-symbol sym) [& args#]
;; ;;        (let [exprs# (map ->expr args#)
;; ;;              new-expr# (cons '~sym exprs#)
;; ;;              const?# (all-const-sigs? args#)
;; ;;              ]
;; ;;          (-> (make-signal new-expr#)
;; ;;              (assoc :const? const?#)))))

;; ;;   (comment
;; ;;     (defop +)
;; ;;     (+ 1 2 t)

;; ;;     (pprint (macroexpand-1 '(defop +)))
;; ;;     (sample-at (+ t 1) 1)

;; ;;     )



;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;;;;;;;;;;;;;; Danger Zone
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; ;;   ;; TODO doesn't seem to work as an alias
;; ;;   (def Vector clojure.lang.PersistentVector)

;; ;;   ;; Parameters
;; ;;   (def time-type Double)

;; ;;   (def time-variable ::time)

;; ;;   (def signal-default-type Double)
;; ;;   (def signal-default-metadata {})
















;; ;; ;;;; Expr Types
;; ;;   ;; Doc:
;; ;;   ;; Expressions are symbolic graphs which can be sampled for a value
;; ;;   ;; at a specific point in time.
;; ;;   ;; - Mono Function Expressions are symbolic representations of function that
;; ;;   ;; - expects a single argument.
;; ;;   ;; - Constant Expressions will always return the same value, and are therefore
;; ;;   ;; independent of the time variable (which they do not contain).
;; ;;   ;; - Time Expressions may return different values at different moments in time
;; ;;   ;; and will therefore contain the time variable (see above).
;; ;;   ;; (defrecord Mono-Func-Expr [^clojure.lang.PersistentList expr, ^Type-Signature type])


;; ;;   ;; Constructors
;; ;;   ;; (defn make-Mono-Func-Expr
;; ;;   ;;   ([expr]
;; ;;   ;;    (make-Mono-Func-Expr expr :type/unknown))
;; ;;   ;;   ([expr type]
;; ;;   ;;    (Mono-Func-Expr. expr type)))


;; ;;   ;; Protocols
;; ;;   ;; (extend-type Mono-Func-Expr
;; ;;   ;;   P-Sig-Functor
;; ;;   ;;   (fmap-output [input-expr f-expr]
;; ;;   ;;     )
;; ;;   ;;   )


;; ;; ;;;; Doc:
;; ;;   ;; `expr` can be one of:
;; ;;   ;; Const-Expr, TimeExpr
;; ;;   ;;


;; ;;   ;; (defn symbolic-apply-to-output [input-f wrapper-f]
;; ;;   ;;   (let [outer-time-arg (gensym "time_input")]
;; ;;   ;;     (list 'fn [outer-time-arg]
;; ;;   ;;           (list apply wrapper-f
;; ;;   ;;                 (list
;; ;;   ;;                  (list apply input-f outer-time-arg))))))


;; ;;   ;; OLD
;; ;;   ;; (defn symbolic-apply-to-output
;; ;;   ;;   "Takes symbolic functions, outputs symbolic functions"
;; ;;   ;;   [input-f wrapper-f]
;; ;;   ;;   (let [outer-time-arg (gensym "time_input")]
;; ;;   ;;     (list 'fn [outer-time-arg]
;; ;;   ;;           (symbolic-apply wrapper-f (symbolic-apply input-f outer-time-arg)))))



;; ;;   ;; ;; Expr-Function :: Expr -> Expr
;; ;;   ;; (defrecord Expr-Function [expr, f]
;; ;;   ;;   P-Sig-Functor
;; ;;   ;;   (fmap-input [this f]
;; ;;   ;;     (let []
;; ;;   ;;       (Expr. () type))))

;; ;;   ;; (extend-type Const-Expr
;; ;;   ;;   P-Sig-Functor
;; ;;   ;;   (fmap-in [this _]
;; ;;   ;;     this)
;; ;;   ;;   (fmap-out [{:keys [expr type] :as old-expr} f]
;; ;;   ;;     (-> old-expr
;; ;;   ;;         (assoc :type type)
;; ;;   ;;         (assoc :expr ))))

;; ;;   ;; (extend-type Time-Expr
;; ;;   ;;   P-Sig-Functor
;; ;;   ;;   (fmap-in [this _]
;; ;;   ;;     this)
;; ;;   ;;   (fmap-out [{:keys [expr type] :as old-expr} f]
;; ;;   ;;     (-> old-expr
;; ;;   ;;         (assoc :type type)
;; ;;   ;;         (assoc :expr ()))))

;; ;;   ;; (extend-type Expr
;; ;;   ;;   P-Sig-Functor

;; ;;   ;;   P-Symbolic-Apply
;; ;;   ;;   (sym-apply [this args]
;; ;;   ;;     ())
;; ;;   ;;   )



;; ;;   ;; Signal-Function Type
;; ;;   ;; Signal-Function a :: Time -> a
;; ;;   ;; Doc:
;; ;;   ;; Every Signal Function consists of `expr`, a symbolic representation,
;; ;;   ;; `f`, the actual call-able function, and `type`, a type signature.
;; ;;   ;; (defrecord Signal-Function [expr f type])

;; ;;   ;; (defn Sig-Func->expr [sf] (:expr sf))
;; ;;   ;; (defn Sig-Func->f [sf] (:f sf))

;; ;;   ;; ;; Constructor
;; ;;   ;; (defmacro make-Signal-Function
;; ;;   ;;   ([expr f type] `(Signal-Function. ~expr ~f ~type))
;; ;;   ;;   ([expr type] `(Signal-Function. ~expr ~(eval expr) ~type))
;; ;;   ;;   ([expr] `(Signal-Function. ~expr ~(eval expr) ~:type/unknown)))

;; ;;   ;; (defn Signal-Function->sig-func [sf]
;; ;;   ;;   (:f sf))

;; ;;   ;; (extend-type Signal-Function
;; ;;   ;;   clojure.lang.IFn
;; ;;   ;;   (invoke [this time]
;; ;;   ;;     ((Sig-Func->f this) time))
;; ;;   ;;   (applyTo [this args]
;; ;;   ;;     (apply (Sig-Func->f this) args))

;; ;;   ;;   P-Sig-Func
;; ;;   ;;   (sig-func [sf]
;; ;;   ;;     (Signal-Function->sig-func sf))

;; ;;   ;;   P-Samplable
;; ;;   ;;   (sample-at [sf time]
;; ;;   ;;     ((sig-func sf) time))

;; ;;   ;;   P-Unboxable
;; ;;   ;;   (unbox [sf] (sig-func sf))

;; ;;   ;;   P-Sig-Functor


;; ;;   ;;   (fmap-out [this f-out]
;; ;;   ;;     (Signal-Function.
;; ;;   ;;      (fn [time]
;; ;;   ;;        (f-out (f time)))))

;; ;;   ;;   )



;; ;;   (def time-input-symbol 'TIME_INPUT)

;; ;;   ;; TODO decide on this
;; ;;   ;; (def identity-signal-function
;; ;;   ;;   (let [expr (Time-Expr.
;; ;;   ;;               (util/strip-symbols
;; ;;   ;;                (let [arg-sym (gensym (name time-input-symbol))]
;; ;;   ;;                  `(fn [~arg-sym] ~arg-sym)))
;; ;;   ;;               ~time-type)]
;; ;;   ;;     (Signal-Function. expr (eval expr) type)))

;; ;;   ;; (let [arg-sym (gensym (name time-input-symbol))]
;; ;;   ;;                  `(fn [~arg-sym] ~arg-sym))


;; ;;   ;; (def identity-signal)

;; ;;   ;; (def identity-signal-function (list time-input-symbol nil))
;; ;;   ;; (def identity-signal-function ::t)

;; ;;   ;; (defn is-time-variable? [form]
;; ;;   ;;   (= (first form) time-input-symbol))

;; ;;   ;; (defn replace-time-variable [form val]
;; ;;   ;;   ;; TODO @performance difference between post- and prewalk?
;; ;;   ;;   (clojure.walk/postwalk #(if (is-time-variable? %) val %) form))

;; ;;   ;; (defn apply-to-time-variable [form f]
;; ;;   ;;   (clojure.walk/postwalk #(if (is-time-variable? %) (f %) %) form))

;; ;;   ;; (defmacro signal [body]
;; ;;   ;;   `(Signal. '~body))

;; ;;   ;; ;; Constructors
;; ;;   ;; (defn make-signal [graph]
;; ;;   ;;   (Signal. graph))

;; ;;   ;; (defn make-constexpr [graph]
;; ;;   ;;   (ConstExpr. graph))
;; ;;   ;; ;; \Constructors

;; ;;   ;; (defn signal? [x] (instance? Signal x))

;; ;;   ;; (defn constexpr? [x]
;; ;;   ;;   (not (signal? x)))


;; ;;   (defn fn-expr-args [expr]
;; ;;     (assert (util/is-symbolic-fn? expr))
;; ;;     (second expr))



;; ;;   ;; TODO this isn't gonna work, is it?
;; ;;   ;; (defmacro make-args [num-args]
;; ;;   ;;   (if (= num-args :variable)
;; ;;   ;;     '[& args#]
;; ;;   ;;     (loop [i 1 acc '[]]
;; ;;   ;;       (if (> i num-args) acc
;; ;;   ;;           (recur
;; ;;   ;;            (inc i)
;; ;;   ;;            (conj acc
;; ;;   ;;                  (symbol (str "arg" i "#"))))))))

;; ;;   ;; (defn make-signal-op [sym]
;; ;;   ;;   (fn [& args]
;; ;;   ;;     (Signal. (cons sym (map graph args)))))





;; ;;   ;; (defn lift-static-signal [val]
;; ;;   ;;   '(fn [t] val))


;; ;;   ;; (defn sig-func
;; ;;   ;;   "Get a callable expr of a signal's function"
;; ;;   ;;   [sig]
;; ;;   ;;   (let [sig-func (if (is-static-signal? sig)
;; ;;   ;;                    (lift-static-signal sig)
;; ;;   ;;                    (:sig-func sig))]
;; ;;   ;;     (eval sig-func)))



;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;   ;; DEPRECATED
;; ;;   ;; OLD & BAD
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;   ;; (defn make-signal [f, sr]
;; ;;   ;;   {:type :signal
;; ;;   ;;    :func f
;; ;;   ;;    ;; :sample-rate sr
;; ;;   ;;    :sr sr})

;; ;;   ;; (defn make-continuous-domain [start, end]
;; ;;   ;;   {:type :continuous-domain
;; ;;   ;;    :start start
;; ;;   ;;    :end end
;; ;;   ;;    :dur (- end start)})

;; ;;   ;; (defn make-discrete-domain [start end sr]
;; ;;   ;;   {:discrete-domain (range start end (/ 1 sr))})

;; ;;   ;; (defn make-timeline
;; ;;   ;;   ([f start end sr]
;; ;;   ;;    (make-timeline (make-signal f sr)
;; ;;   ;;                   (make-continuous-domain start end)))
;; ;;   ;;   ([{:keys [sr] :as sig}
;; ;;   ;;     {:keys [start end] :as domain}]
;; ;;   ;;    (merge sig
;; ;;   ;;           domain
;; ;;   ;;           (make-discrete-domain start end sr)
;; ;;   ;;           {:type :timeline})))


;; ;;   ;; (defn sample-timeline [{:keys [func discrete-domain] :as tml}]
;; ;;   ;;   (assoc tml :vals (map func discrete-domain)))


;; ;;   ;; (defmacro defsignal [name args-vec body]
;; ;;   ;;   `(def ~name
;; ;;   ;;      (Signal. (quote (fn ~args-vec ~body))
;; ;;   ;;               ~signal-default-type
;; ;;   ;;               ~signal-default-metadata)))


;; ;;   ;; (defsignal uni-to-bi [sig]
;; ;;   ;;   (- (* 2.0 sig) 1.0))


;; ;;   ;; (comment
;; ;;   ;;   (pprint (macroexpand-1
;; ;;   ;;            '(defsignal uni-to-bi [sig]
;; ;;   ;;               (- (* 2.0 sig) 1.0))))

;; ;;   ;;   )

;; ;;   ;; TODO should I keep this?
;; ;;   ;;   clojure.lang.IFn
;; ;;   ;;   (invoke [this arg]
;; ;;   ;;     ((eval sig-func-expr) arg))
;; ;;   ;;   (applyTo [this args]
;; ;;   ;;     (apply (eval sig-func-expr) args)))


;; ;;   ;; NOTE num-args can be :variable
;; ;;   ;; (defmacro define-signal-operator [symbol num-args]
;; ;;   ;;   (let [arglist
;; ;;   ;;         '[& args#]
;; ;;   ;;         ;; (case num-args
;; ;;   ;;         ;;   :variable '[])
;; ;;   ;;         ]
;; ;;   ;;   `(defn ~symbol ~arglist )
;; ;;   ;;   ))






;; ;;   ;; Defining signal operators
;; ;;   ;; TODO @robustness test these
;; ;;   ;; (defmacro def-sig-un-op [name op-sym]
;; ;;   ;;   `(defn ~name [arg#]
;; ;;   ;;      (let [constructor# (appropriate-constructor arg#)
;; ;;   ;;            form# (cons ~op-sym
;; ;;   ;;                        (list (unbox arg#)))]
;; ;;   ;;        (constructor# form#))))

;; ;;   ;; (defmacro def-sig-bin-op [name op-sym]
;; ;;   ;;   `(defn ~name [arg1# arg2#]
;; ;;   ;;      (let [constructor# (appropriate-constructor (list arg1# arg2#))
;; ;;   ;;            form# (cons ~op-sym
;; ;;   ;;                        (map unbox (list arg1# arg2#)))]
;; ;;   ;;        (constructor# form#))))

;; ;;   ;; (defmacro def-sig-vari-op [name op-sym]
;; ;;   ;;   `(defn ~name [& args#]
;; ;;   ;;      (let [constructor# (appropriate-constructor args#)
;; ;;   ;;            form# (cons ~op-sym
;; ;;   ;;                        (list (map unbox args#)))]
;; ;;   ;;        (constructor# form#))))

;; ;;   ;; TODO
;; ;;   (defn partially-evaluate-graph-at [graph time]
;; ;;     (clojure.walk/postwalk ))

;; ;;   ;; DEPRECATED
;; ;;   ;; ;; ConstExpr
;; ;;   ;; ;;;;;;;;;;;;;;;
;; ;;   ;; (defrecord ConstExpr [graph]
;; ;;   ;;   P-Samplable
;; ;;   ;;   (sample-at [_ _]
;; ;;   ;;     ;; TODO should the graph be evaled or not?
;; ;;   ;;     (eval graph))

;; ;;   ;;   (signal-type
;; ;;   ;;     ;; ([_] (class graph))
;; ;;   ;;     [_  _] (type graph))

;; ;;   ;;   P-Unboxable
;; ;;   ;;   (unbox [_] graph))

;; ;;   )
