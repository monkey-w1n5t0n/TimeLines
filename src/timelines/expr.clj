(ns timelines.expr
  (:require [timelines.protocols :refer :all]
            [timelines.utils :as util]
            [clojure.spec.alpha :as s]
            [timelines.specs :as ts]))

(declare symbolic-apply-direct
         symbolic-apply-indirect
         symbolic-apply-direct-or-indirect
         symbolic-higher-order-apply-to-input
         symbolic-higher-order-apply-to-output)

(defn parse-fn [e]
  (s/conform :clojure/lambda-expr e))

(defn fn-args [e]
  (->> e parse-fn :args))

(defn fn-return-e [e]
  (->> e parse-fn :body last))

(defn fn-body [e]
  (->> e (s/conform :clojure/lambda-expr) :body))

(defn fn? [e]
  (s/valid? :clojure/lambda-expr e))

(defn ->clojure-fn
  "Returns an actually `eval`able expression"
  [e]
  (let [{:keys [args body]} (parse-fn e)]
    (concat
     (list 'clojure.core/fn (into [] args))
     body)))

;; TODO  @correctness @performance
;; Useful for being able to tell whether functions are
;; functionally equivalent even if their exprs differ slightly
(defn simplify [expr]
  expr)

(defn sigfn? [expr]
  (and (-> expr simplify fn?)
       (= 1 (count (second expr)))))

(defn sigfn->time-arg [expr]
  (-> expr second first))

(defn sigfn->replace-time-arg [sigfn new-time-arg-sym]
  (let [old-time-arg (sigfn->time-arg sigfn)
        old-body (nth sigfn 2)]
    (util/replace-sym old-body old-time-arg new-time-arg-sym)))

;; (defn postmap [post-f f]
;;   (update-body f ()))
