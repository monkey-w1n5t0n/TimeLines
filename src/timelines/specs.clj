(ns timelines.specs
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [timelines.signal :as sig]
            [timelines.utils :as u]))

;; basic Clojure stuff
;; Specs that start with :clojure are clojure-specific (e.g. arglists of functions have to be vectors, not lists)
;; The rest are more general
(do
  (s/def :clojure/set set?)
  (s/def :clojure/map map?)
  (s/def :clojure/symbol symbol?)

  (s/def :clojure.expr.fn-call/arg any?)
  (s/def :clojure.expr.fn-call/args (s/* :clojure/arg))
  ;; What you'd expect to see in (fn <here> ...)
  (s/def :clojure.expr.fn/arglist (s/and vector?
                                         (s/coll-of simple-symbol? :distinct true :into [])))
  (s/def :expr.fn/arglist (s/coll-of simple-symbol? :distinct true :into []))

  (s/def :clojure.expr/arglist :clojure.expr.fn/arglist)
  ;; Multiple arglists
  (s/def :clojure.expr.fn/arglists (s/coll-of :clojure/arglist))
  (s/def :clojure.expr/arglists :clojure.expr.fn/arglists)

  (s/def :clojure/expr any?)
  (s/def :expr/any any?)

  (s/def :clojure.expr.fn/body (s/* :clojure/expr))
  (s/def :expr.fn/body (s/* :expr/any))

  (s/def :clojure/fn clojure.core/fn?)

  (s/def :clojure.expr/fn
    (s/cat :fn   #{'fn 'clojure.core/fn}
           :args :clojure.expr.fn/arglist
           :body :clojure.expr.fn/body))

  (s/def :expr/lambda
    (s/cat :fn   #{'fn 'clojure.core/fn}
           :args :expr.fn/arglist
           :body :expr.fn/body))

  ;; TODO @correctness check whether it satisfies IFn (or whatever it's called) instead?
  (s/def :clojure.expr/callable
    ^{:doc "Anything that can be considered a valid function (i.e. that implements clojure.lang.IFn),
or anything that can be evaluated to one."}
    (s/or :symbol symbol?
          :map map?
          :vector vector?
          :expr/fn :clojure.expr/fn
          :fn :clojure/fn))

  ;; TODO @robustness maybe the key shouldn't be fn because
  ;; if destructured it shadows (fn [...] ...)?
  (s/def :clojure.expr/fn-call
    ^{:doc "Anything that can be considered a valid function call, including things like maps or vectors."}
    (s/and list?
           (s/cat :fn :clojure.expr/callable
                  :args :clojure/args))))

(comment

  (s/conform :clojure.expr/callable '(fn [x] x))

  (s/conform :clojure.expr/fn-call '((fn [x y z]
                                       (+ x y z))
                                     1 2 3)))

;; Signal stuff
(do
  (s/def ::sig-func
    ^{:doc "An unevaluated lambda expression with just one argument."}
    (s/and :clojure/lambda-expr
           ;; #(= 1 (-> % second count))
           #(= 1 (-> % :args count))))

  (comment))

;; FIXME
;; (s/def ::signal (s/or :sig (instance? timelines.signal.core/Signal)
;;                       :sig-func ::sig-func)))

;; Project-level stuff
(do
  ;; TODO @completeness extend this to more rich representations
  (s/def :timelines/doc string?))

;; Objects
(do

  (s/def :thing/id any?)

  (s/def :behavior/trigger keyword?)

  (s/def :behavior/triggers (s/coll-of :behavior/trigger :distinct true :into #{}))

  ;; TODO @flexibility maybe allow more types?
  (s/def :behavior/name keyword?)

  (s/def :behavior/action :clojure/fn)

  (s/def :thing/behavior (s/keys :req-un [:behavior/name :behavior/triggers :behavior/action]
                                 :opt-un [:timelines/doc]))

  (s/def :thing/behavior-list (s/* :thing/behavior))

  (comment

    (s/explain :thing/behavior {:name :hohoho :triggers #{:bob :dave} :action '(fn [a b c] (+ a b c))}))
  ;; TODO @completeness add more
  ;; FIXME
  ;; (s/def ::thing )
  )

;; ECS
(do
  ;; Attribute
  (s/def :attribute/name string?)
  (s/def :attribute/data any?)
  (s/def :entity/attribute (s/keys :req-un [:attribute/name :attribute/data]))

  ;; Entity
  (s/def :entity/id nat-int?)
  (s/def :entity/name string?)
  (s/def :entity/attributes (s/map-of :attribute/name :entity/attribute))
  (s/def :timelines/entity  (s/keys :req-un [:entity/id :entity/attributes]
                                    :opt-un [:entity/name]))
  ;; System
  (s/def :system/required-attributes (s/coll-of :attribute/name :distinct true :into #{}))
  (s/def :system/affected-attributes (s/coll-of :attribute/name :distinct true :into #{}))
  (s/def :timelines/system (s/keys :req-un [:system/name :system/required-attributes :system/affected-attributes :system/action])))

;; Sessions
(do

  (s/def :node/name string?)
  (s/def :node/id int?)
  (s/def :node/id int?))
