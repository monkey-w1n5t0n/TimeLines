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

(def init-time-arg 't_0)

(def id-sigfn (list 'clojure.core/fn [init-time-arg] init-time-arg))

(defn fn? [e] (s/valid? :clojure.expr/fn e))

(defn parse-fn [e]
  (s/conform :clojure.expr/fn e))

(defn fn-args [e]
  (->> e parse-fn :args))

(defn fn-return-e [e]
  (->> e parse-fn :body last))

(defn fn-bodies [e]
  (->> e parse-fn :body))

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
