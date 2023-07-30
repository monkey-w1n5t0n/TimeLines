(ns timelines.expr
  (:require [timelines.protocols :refer :all]
            [timelines.types :refer :all]
            [timelines.utils :as util]))

(declare symbolic-apply-direct
         symbolic-apply-indirect
         symbolic-apply-direct-or-indirect
         symbolic-higher-order-apply-to-input
         symbolic-higher-order-apply-to-output)

(defn fn? [expr]
  (and (instance? clojure.lang.PersistentList expr)
       ;; TODO optionally many bodies
       (= 3 (count expr))
       (= 'fn (util/strip-symbol-ns-qualifiers (first expr)))
       (instance? clojure.lang.PersistentVector (second expr))
       ;; (instance? clojure.lang.PersistentVector (second expr))
       ))

(defn assert-fn [expr]
  (if (fn? expr)
    true
    (throw (AssertionError. (str "Not proper fn expression: " expr)))))

;; TODO big
(defn simplify [expr]
  expr)

;; TODO more robust testing
;;

(defn sigfn? [expr]
  (and (-> expr simplify fn?)
       (= 1 (count (second expr)))))

(defn assert-sigfn [expr]
  (if (sigfn? expr)
    true
    (throw (AssertionError. (str "Not proper Signal fn expression: " expr)))))

(defn sigfn->time-arg [expr]
  (assert-sigfn expr)
  (-> expr second first))

(defn sigfn->replace-time-arg [sigfn new-time-arg-sym]
  (let [old-time-arg (sigfn->time-arg sigfn)
        old-body (nth sigfn 2)]
    (util/replace-sym old-body old-time-arg new-time-arg-sym)))
