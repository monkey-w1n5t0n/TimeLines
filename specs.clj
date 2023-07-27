(ns timelines.specs
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [timelines.util.core :as u]))

(s/def ::id any?)
(s/def ::set set?)
(s/def ::map map?)

(s/def ::trigger keyword?)

(s/def ::triggers (s/coll-of ::trigger
                             :into #{}))

(s/def ::arg any?)
(s/def ::args (s/* ::arg))
(s/def ::arg-specifier symbol?)
(s/def ::fn-arglist (s/coll-of ::arg-specifier))

(s/def ::expr any?)

(s/def ::fn-body (s/* ::expr))

(defn contained-in? [item coll]
  (contains? coll item))

(defn one-of? [item coll]
  (contains? coll item))

(s/def ::lambda #(instance? clojure.lang.AFunction %))

(s/def ::lambda-expr (s/and sequential?
                            (s/cat :fn #(one-of? % #{'fn 'clojure.core/fn})
                                   :args ::fn-arglist
                                   :body ::fn-body)))

;; TODO @correctness check whether it satisfies IFn (or whatever it's called) instead?
(s/def ::fn
  ^{:doc "Anything that can be considered a valid function, i.e. that implements clojure.lang.IFn."}
  (s/or :symbol symbol?
        :map map?
        :vector vector?
        :lambda-expr ::lambda-expr
        :lambda ::lambda))

;; TODO @robustness maybe the key shouldn't be fn because
;; if destructured it shadows (fn [...] ...)?
(s/def ::fn-call
  ^{:doc "Anything that can be considered a valid function call, including things like maps or vectors."}
  (s/cat :fn ::fn
         :args ::args))

(s/def ::sig-func
  ^{:doc "An unevaluated lambda expression with just one argument."}
  (s/and ::lambda-expr
         ;; #(= 1 (-> % second count))
         #(= 1 (-> % :args count))))
