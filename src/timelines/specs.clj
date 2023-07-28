(ns timelines.specs
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [timelines.signal.core :as sig]
            [timelines.util.core :as u]))

;; Helper utils
(do
  (defn atom? [x]
    (instance? clojure.lang.Atom x))

  (defn contained-in? [item coll]
    (contains? coll item))

  (defn one-of? [item coll]
    (contains? coll item)))

;; basic Clojure stuff
(do
  (s/def ::set set?)
  (s/def ::map map?)

  (s/def ::arg any?)
  (s/def ::args (s/* ::arg))
  (s/def ::arg-specifier symbol?)
  (s/def ::fn-arglist (s/coll-of ::arg-specifier))

  (s/def ::expr any?)

  (s/def ::fn-body (s/* ::expr))

  (s/def ::lambda #(instance? clojure.lang.AFunction %))

  (s/def ::lambda-expr (s/and sequential?
                              (s/cat :fn #(one-of? % #{'fn 'clojure.core/fn})
                                     :args ::fn-arglist
                                     :body ::fn-body)))

;; TODO @correctness check whether it satisfies IFn (or whatever it's called) instead?
  (s/def ::fn
    ^{:doc "Anything that can be considered a valid function (i.e. that implements clojure.lang.IFn),
or anything that can be evaluated to one."}
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
           :args ::args)))

;; Signal stuff
(do
  (s/def ::sig-func
    ^{:doc "An unevaluated lambda expression with just one argument."}
    (s/and ::lambda-expr
           ;; #(= 1 (-> % second count))
           #(= 1 (-> % :args count))))

  ;; FIXME
  ;; (s/def ::signal (s/or :sig (instance? timelines.signal.core/Signal)
  ;;                       :sig-func ::sig-func)))

  ;; Objects
  (do

    (s/def :thing/id any?)

    (s/def :behavior/trigger keyword?)

    (s/def :behavior/trigger-set (s/coll-of ::trigger
                                            :into #{}))

    ;; TODO @flexibility maybe allow more types?
    (s/def :behavior/name keyword?)

    (s/def :thing/behavior (s/cat :name ::behavior-name
                                  :triggers ::behavior-trigger-set
                                  :action ::fn)
      #_(s/keys :req-un [:name]))

    (s/def :thing/behavior-list (s/* :thing/behavior))

    ;; TODO @completeness add more
    ;; FIXME
    ;; (s/def ::thing )
    )
