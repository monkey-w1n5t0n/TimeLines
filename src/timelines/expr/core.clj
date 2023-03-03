(ns timelines.expr.core
  (:require [timelines.protocols :refer :all]
            [timelines.types :refer :all]
            [clojure.pprint :refer [pprint]]
            [timelines.util.core :as util]
            ))


(declare symbolic-apply-direct
         symbolic-apply-indirect
         symbolic-apply-direct-or-indirect
         symbolic-higher-order-apply-to-input
         symbolic-higher-order-apply-to-output
         )

(defrecord Const-Expr [expr, type]
  P-Sig-Functor
  ;; Doesn't do anything to a Const-Expr
  (fmap-input [this _]
    this)
  (fmap-output [this sym-f]
    (assoc this :expr
           (symbolic-apply-direct-or-indirect sym-f expr))))


(defrecord Time-Expr [expr, type]
  P-Sig-Functor
  ;; Doesn't do anything to a Const-Expr
  (fmap-input [this sym-f]
    (assoc this :expr
           (symbolic-higher-order-apply-to-input sym-f expr)))
  (fmap-output [this sym-f]
    (assoc this :expr
           (symbolic-higher-order-apply-to-output sym-f expr))))


(defn make-Const-Expr
  ([expr]
   (make-Const-Expr expr :type/unknown))
  ([expr type]
   (Const-Expr. expr type)))

(defn make-Time-Expr
  ([expr]
   (make-Time-Expr expr :type/unknown))
  ([expr type]
   (Time-Expr. expr type)))


;;;;;;;; Helpers
(defn apply-to [f & args]
  (clojure.core/apply f args))

(defn symbolic-apply-direct
  "Symbolically apply a function to arguments without using `apply`"
  [symbolic-f & symbolic-args]
  (util/strip-symbols
   `(~symbolic-f ~@symbolic-args)))

(defn symbolic-apply-indirect
  "Symbolically apply a function to arguments using `apply-to`"
  [symbolic-f & symbolic-args]
  (util/strip-symbols
   `(apply-to ~symbolic-f ~@symbolic-args)))


(defn choose-symbolic-apply-method
  "Decide whether to apply a symbolic function directly or indirectly"
  [sym-f]
  (if (util/is-symbolic-fn? sym-f)
    #'symbolic-apply-indirect
    #'symbolic-apply-direct))


(defn symbolic-apply-direct-or-indirect [sym-f & sym-args]
  (apply (choose-symbolic-apply-method sym-f)
         (cons sym-f sym-args)))


;;;
(defn symbolic-higher-order-apply-to-input
  "Takes symbolic functions, outputs symbolic functions"
  [f time-f]
  (util/strip-symbols
   `(fn [time_input#]
      (apply-to ~f (apply-to ~time-f time_input#)))))

(defn symbolic-higher-order-apply-to-output
  "Takes symbolic functions, outputs symbolic functions"
  [f time-f]
  (util/strip-symbols
   `(fn [time_input#]
      (apply-to ~f (apply-to ~time-f time_input#)))))


;; Other AST stuff
(defrecord LetExpr [bindings body])

(defn expr->is-let? [expr]
  (and (= 'let (first expr))
       (instance? clojure.lang.PersistentVector (second expr))))

(defn expr->let-bindings [expr]
  (second expr))

(letfn [(flatten-and-filter-fn->set [pred name]
          (fn [in]
            (if (pred in) #{in}
                (->> in seq flatten (filter pred) set))))]
  (def expr->symbol-literal-set (flatten-and-filter-fn->set symbol? "expr-symbol-set"))
  (def expr->keyword-literal-set (flatten-and-filter-fn->set keyword? "expr-keyword-set"))
  (def expr->string-literal-set (flatten-and-filter-fn->set string? "expr-keyword-set")))
