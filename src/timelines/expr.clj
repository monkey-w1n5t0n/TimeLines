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

(defn parse [spec e]
  (assoc
   (s/conform spec e)
   ::parsed? true))

(s/conform int? "hi")

(defn enforce [spec e]
  (if (::parsed? e)
    e
    (let [conformed (s/conform spec e)]
      (if-not (= :clojure.spec.alpha/invalid conformed)
        conformed
        (do
          (s/explain spec e)
          (throw (Exception. "Spec mismatch, see above")))))))
(defn parse-fn [e]
  ((s/conform :clojure.expr/fn e)
   :expr/parsed? true))

(defn fn? [e] (s/valid? :clojure.expr/fn e))

(defn fn-args [e]
  (->> e parse-fn :args))

(defn fn-return-e [e]
  (->> e parse-fn :body last))

(defn fn-body [e]
  (->> e (s/conform :clojure/lambda-expr) :body))

(defn ->clojure-fn
  "Returns an actually `eval`able expression"
  [e]
  (let [{:keys [args body]} (parse-fn e)]
    (concat
     (list 'clojure.core/fn (into [] args))
     (into '() body))))

(s/unform :clojure.expr/fn
          (s/conform :clojure.expr/fn '(fn [x] (+ x 1))))

;; (->clojure-fn {:args '[x y] :body ['(+ x y)]})

;; TODO  @correctness @performance
;; Useful for being able to tell whether functions are
;; functionally equivalent even if their exprs differ slightly
(defn simplify [expr]
  expr)

(defn parse-sigfn [e]
  (s/conform :expr.sig/fn e))

(def ->sigfn ->clojure-fn)

(defn sigfn? [e] (s/valid? :expr.sig/fn e))

(defn time-arg [e]
  (-> e parse-sigfn :args first))

(defn sigfn->replace-time-arg [sigfn new-time-arg-sym]
  (let [old-time-arg (time-arg sigfn)
        old-body (nth sigfn 2)]
    (util/replace-sym old-body old-time-arg new-time-arg-sym)))

;; (defn postmap [post-f f]
;;   (update-body f ()))

;; Macro idea for defn that supports opts for args
;; to automatically ensure them
(comment
  (defmacro defn [& args]
    (let [{:keys [name args body opts]}
          (s/conform (s/cat :name simple-symbol?
                            :opts (s/* keyword?)
                            :args :clojure.expr.fn/arglist)
                     args)]
      `(~'defn ~name ~args ~@body)))

  (s/conform (s/cat :name simple-symbol?
                    :opts (s/* keyword?)
                    :args-and-opts (s/* N)
                    :body :clojure.expr.fn/body)
             '(foo [x y z] (+ x y z)))

  (s/conform (s/* (s/alt :sym symbol?
                         :sym+opt (s/cat :sym symbol? :opt keyword?)))
             '[x y :clojure.expr z])

  (defn time-arg [e :expr.fn/arglist y :expr/fn]
    (-> e parse-sigfn :args first))

  (macroexpand-1 '(defn test [x]
                    (-> x))))
