(ns timelines.expr
  (:require
   [timelines.protocols :as p]    ;
   [timelines.utils :as util]
   [clojure.spec.alpha :as s]
   [timelines.specs :as ts]))

;; TODO should implement both premap and postmap under bimap
;; increment time arg every time something is bimapped

(declare symbolic-apply-direct
         symbolic-apply-indirect
         symbolic-apply-direct-or-indirect
         symbolic-higher-order-apply-to-input
         symbolic-higher-order-apply-to-output)

(defn parse-fn [e]
  (s/conform :clojure.expr/fn e))

(defn fn? [e] (s/valid? :clojure.expr/fn e))

(defn fn-args [e]
  (->> e parse-fn :args))

(defn fn-return-e [e]
  (->> e parse-fn :body last))

(defn fn-body [e]
  (->> e (s/conform :clojure/lambda-expr) :body))

;; (->clojure-fn {:args '[x y] :body ['(+ x y)]})

;; TODO  @correctness @performance
;; Useful for being able to tell whether functions are
;; functionally equivalent even if their exprs differ slightly
(defn simplify [expr]
  expr)

(defn parse-sigfn [e]
  (s/conform :expr.sig/fn e))

(defn sigfn? [e] (s/valid? :expr.sig/fn e))

(defn time-arg [e]
  (-> e parse-sigfn :args first))

(defn sigfn->replace-time-arg [sigfn new-time-arg-sym]
  (let [old-time-arg (time-arg sigfn)
        old-body (nth sigfn 2)]
    (util/replace-sym old-body old-time-arg new-time-arg-sym)))

;; (defn postmap [post-f f]
;;   (update-body f ()))

(defn apply' [f x]
  (f x))

;; TODO @properness these are just the same with the only thing different being the
;; order of whether the time arg gets put through e or f first, before being put through the other
(defn sigfn-premap [e f]
  (let [time-arg (-> e second first)
        f-is-fn? (some (set '[fn clojure.core/fn])
                       (list (first f)))
        f (if f-is-fn? (list f) f)
        e (list e)
        new-body (list '-> time-arg f e)
        new-expr (list 'fn [time-arg] new-body)]
    new-expr))

(defn sigfn-postmap [e f]
  (let [time-arg (-> e second first)
        f-is-fn? (some (set '[fn clojure.core/fn])
                       (list (first f)))
        f (if f-is-fn? (list f) f)
        e (list e)
        new-body (list '-> time-arg e f)
        new-expr (list 'fn [time-arg] new-body)]
    new-expr))
