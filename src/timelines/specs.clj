(ns timelines.specs
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [timelines.signal :as sig]
            [timelines.utils :as u]))

;; Helper utils

;; basic Clojure stuff
(do
  (s/def :clojure/set set?)
  (s/def :clojure/map map?)
  (s/def :clojure/symbol symbol?)

  (s/def :clojure/arg any?)
  (s/def :clojure/args (s/* :clojure/arg))
  (s/def :clojure/arg-specifier simple-symbol?)
  ;; What you'd expect to see in (fn <here> ...)
  (s/def :clojure/fn-arglist (s/coll-of :clojure/arg-specifier :distinct true :into []))
  (s/def :clojure/arglist :clojure/fn-arglist)
  ;; Multiple arglists
  (s/def :clojure/arglists (s/coll-of :clojure/arglist))

  (s/def :clojure/expr any?)

  (s/def :clojure/fn-body (s/* :clojure/expr))

  (s/def :clojure/lambda #(instance? clojure.lang.AFunction %))

  (s/def :clojure/lambda-expr
    (s/cat :fn   #{'fn 'clojure.core/fn}
           :args :clojure/fn-arglist
           :body :clojure/fn-body))

  ;; TODO @correctness check whether it satisfies IFn (or whatever it's called) instead?
  (s/def :clojure/fn
    ^{:doc "Anything that can be considered a valid function (i.e. that implements clojure.lang.IFn),
or anything that can be evaluated to one."}
    (s/or :symbol symbol?
          :map map?
          :vector vector?
          :lambda-expr :clojure/lambda-expr
          :lambda :clojure/lambda))

  ;; TODO @robustness maybe the key shouldn't be fn because
  ;; if destructured it shadows (fn [...] ...)?
  (s/def :clojure/fn-call
    ^{:doc "Anything that can be considered a valid function call, including things like maps or vectors."}
    (s/and list?
           (s/cat :fn :clojure/fn
                  :args :clojure/args))))

;; Signal stuff
(do
  (s/def ::sig-func
    ^{:doc "An unevaluated lambda expression with just one argument."}
    (s/and :clojure/lambda-expr
           ;; #(= 1 (-> % second count))
           #(= 1 (-> % :args count)))))

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
