(ns timelines.expr
  (:require
   [timelines.protocols :as p]          ;
   [timelines.utils :as u]
   [clojure.spec.alpha :as s]
   [timelines.macros :as m]
   [timelines.specs :as ts]
   [clojure.string :as str]))

;; TODO should implement both premap and postmap under bimap
;; increment time arg every time something is bimapped

(defprotocol Compilable
  (compile [this]))

(extend-protocol Compilable
  Long
  (compile [this] this)

  clojure.lang.BigInt
  (compile [this] this)

  Double
  (compile [this] this)

  BigDecimal
  (compile [this] this)

  clojure.lang.Ratio
  (compile [this] this)

  String
  (compile [this] this)

  Character
  (compile [this] this)

  clojure.lang.Symbol
  (compile [this] this)

  clojure.lang.Keyword
  (compile [this] this)

  Boolean
  (compile [this] this)

  nil
  (compile [this] this)

  clojure.lang.PersistentList
  (compile [this] (into '() (map compile this)))

  clojure.lang.PersistentVector
  (compile [this] (mapv compile this))

  clojure.lang.PersistentArrayMap
  (compile [this] this)

  clojure.lang.PersistentHashMap
  (compile [this] this)

  clojure.lang.PersistentHashSet
  (compile [this] this)

  java.util.regex.Pattern
  (compile [this] this))

;; (foo 1 2 3)
(defrecord fn-call [f args]
  Compilable
  (compile  [this]
    (conj (map compile args) (compile f))))

;; pi = 3.14
(defrecord const-def [name val]
  Compilable
  (compile  [this]
    (list 'def name (compile val))))

;; my-lfo = (sine (* 2 pi t))
(defrecord sig-def [name expr]
  Compilable
  (compile [this]
    (list 'def (:name this) (compile (:expr this)))))

;; my-fn arg1 arg2 = (+ arg1 arg2)
(defrecord fn-def [name lambda]
  Compilable
  (compile [this]
    (list 'defn (:name this)
          (compile (:lambda this)))))

;; fn t = (+ t 1))
(defrecord lambda [args body]
  Compilable
  (compile [this]
    (list 'fn (vec (:args this))
          (compile (:body this)))))

(defrecord let-block [bindings body]
  Compilable
  (compile [this]
    (list 'let (vec (compile (:bindings this)))
          (compile (:body this)))))

(comment
  (compile (->let-block ['a 3 'b (->fn-call '+ [1 2])]
                        '(+ a b)))
  ;;
  )
;; Define specs for our custom types
(s/def ::fn-call (s/keys :req-un [::f ::args]))
(s/def ::const-def (s/keys :req-un [::name ::val]))
(s/def ::sig-def (s/keys :req-un [::name ::expr]))
(s/def ::fn-def (s/keys :req-un [::name ::lambda]))
(s/def ::lambda (s/keys :req-un [::args ::body]))
(s/def ::let-block (s/keys :req-un [::bindings ::body]))

(declare parse)

(defn parse-fn-call [expr]
  (let [[f & args] expr]
    (->fn-call (parse f) (mapv parse args))))

(defn parse-const-def [expr]
  (let [[_ name val] expr]
    (->const-def name (parse val))))

(defn parse-sig-def [expr]
  (let [[_ name expr] expr]
    (->sig-def name (parse expr))))

(defn parse-fn-def [expr]
  (let [[_ name lambda] expr]
    (->fn-def name (parse lambda))))

(defn parse-lambda [expr]
  (let [[_ args body] expr]
    (->lambda args (parse body))))

(defn parse-let-block [expr]
  (let [[_ bindings body] expr]
    (->let-block (partition 2 (map parse bindings)) (parse body))))

(defn parse [expr]
  (cond
    (integer? expr) expr
    (float? expr) expr
    (ratio? expr) expr
    (string? expr) expr
    (char? expr) expr
    (symbol? expr) expr
    (keyword? expr) expr
    (boolean? expr) expr
    (nil? expr) expr
    (list? expr) (cond
                   (= 'def (first expr)) (parse-const-def expr)
                   (= 'defn (first expr)) (parse-fn-def expr)
                   (= 'fn (first expr)) (parse-lambda expr)
                   (= 'let (first expr)) (parse-let-block expr)
                   :else (parse-fn-call expr))
    (vector? expr) (mapv parse expr)
    (map? expr) expr
    (set? expr) expr
    (instance? java.util.regex.Pattern expr) expr
    :else (throw (ex-info "Unknown expression type" {:expr expr}))))

(defprotocol Parseable
  (parse [this]))

(extend-protocol Parseable
  Object
  (parse [this] this)
  clojure.lang.PersistentList
  (parse [this] (parse (seq this)))
  clojure.lang.PersistentVector
  (parse [this] (mapv parse this))
  clojure.lang.PersistentArrayMap
  (parse [this] this)
  clojure.lang.PersistentHashMap
  (parse [this] this)
  clojure.lang.PersistentHashSet
  (parse [this] this))

;;;;
(def init-time-arg 't_0)

(def id-sigfn (->lambda 't 't))

(defn fn? [e] (instance? lambda e))

(defn fn-args [n]
  (if (fn? n)
    (:args n)
    (throw (Exception. "argument is not a valid fn"))))

(defn fn-return-e [n]
  (->> n :lambda :body last))

(defn fn-bodies [n]
  (->> n :body))

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

(defn const? [e]
  (or (not (sigfn? e))
      (u/not-in? (flatten (fn-bodies e))
                 (time-arg e))))

(defn apply' [f x]
  (f x))

(defn incr-time-arg [e]
  (let [[arg-base num] (-> e time-arg name (str/split #"_"))
        new-num (if-not num "1"
                        (-> num Integer/parseInt inc str))]
    (-> arg-base (str "_" new-num) symbol)))

(defn coerce-sigfn [e]
  (if (sigfn? e)
    e
    (list 'clojure.core/fn [init-time-arg] e)))

(defn coerce-thread-fn [e]
  (m/if-pf e fn? list))

;; FIXME
;; Should be equivalent to premap when post is nil, and vice versa
(defn sigfn-bimap [e pre post]
  (let [e (coerce-sigfn e)
        [pre post] (map coerce-thread-fn [pre post])
        new-t (incr-time-arg e)]
    `(fn [~new-t] ~(filter some? `(-> ~new-t
                                      ~pre
                                      (~e)
                                      ~post)))))

;; TODO @properness these are just the same with the only thing different being the
;; order of whether the time arg gets put through e or f first, before being put through the other
(defn sigfn-premap [e f]
  (sigfn-bimap e f nil))

(defn sigfn-postmap [e f]
  (sigfn-bimap e nil f))

(defn thread-first-arg [e t]
  `(-> ~t ~(coerce-thread-fn e)))

(defn apply-postmap [op fs]
  (let [new-t init-time-arg
        process-f (fn [e] (m/if-pf e sigfn? #(thread-first-arg % new-t)))
        args (map process-f fs)]
    `(fn [~new-t] (~op ~@args))))

(comment

  + 1 (+ 1 2) (fn [t] t)

  (fn [t] (+ 1 (+ 1 2) (-> t ((fn [t] t))))))
