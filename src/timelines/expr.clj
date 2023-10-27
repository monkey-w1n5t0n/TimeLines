(ns timelines.expr
  (:require
   [timelines.protocols :as p]          ;
   [timelines.utils :as u]
   [clojure.spec.alpha :as s]
   [timelines.macros :as m]
   [timelines.specs :as ts]
   [clojure.string :as str]
   [flow-storm.api :as fs-api]
   [clojure.pprint :as pp]))

;; TODO should implement both premap and postmap under bimap
;; increment time arg every time something is bimapped

(comment
  (fs-api/local-connect))

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

(declare is-static?)
(declare sigfn?)
(declare time-arg)

;; TODO  @correctness @performance
;; Useful for being able to tell whether functions are
;; functionally equivalent even if their exprs differ slightly
(defn simplify [e]
  (letfn [(is-first-sym?
            ([s]
             (is-first-sym? e s))
            ([e s]
             (-> e first u/strip-symbol (= s))))]
    (if (is-static? e) (eval e)
        e
        #_(let [t (time-arg e)
                body (last (fn-bodies e))]
            (cond
              (is-first-sym? '*) (if (u/in? (rest e) 0)
                                   0
                                   e)
              (sigfn? e) e)))))

(comment
  (is-static? '(+ 1 2))

  (simplify '(fn [t] (+ 1 2 t)))

  a                                     ;
  )

(defn parse-sigfn [e]
  (s/conform :expr.sig/fn e))

(defn sigfn? [e] (s/valid? :expr.sig/fn e))

(defn time-arg [e]
  (-> e parse-sigfn :args first))

(defn is-static?
  ([e]
   (let [conformed (parse-fn e)]
     (if (= conformed :clojure.spec.alpha/invalid)
       ;; It's not a sigfn
       true
       ;; Assumed to be a '(fn [t_n] ...)` expr
       (is-static? (-> e fn-bodies last)
                   (time-arg e)))))
  ;; Find whether a body is static
  ;; with respect to a specific time symbol
  ([body time]
   (cond
     (= body time)
     false

     (or (list? body)
         (instance? clojure.lang.Cons body))
         ;; Check if we're passing time to another sigfn,
         ;; which then also needs to be examined
     (if
      (= (-> body first u/strip-symbol-ns-qualifiers)
         'pass-time-to)
       (let [time-being-passed (second body)
             new-sigfn (-> body (nth 2))
             new-sigfn-body (-> new-sigfn fn-bodies last)
             new-sigfn-time (time-arg new-sigfn)]
         (if (not (is-static? time-being-passed time))
           (is-static? new-sigfn-body new-sigfn-time)
           true))
             ;; We're not passing time here (but we may further down the tree!)
             ;; Just loop over all exprs and check whether thei're static in respect to the current time
       (u/all? #(is-static? % time) body))
     :default true)))

(defn const-sigfn? [e]
  (and (sigfn? e) (is-static? e)))

(defn const? [e]
  (or (not (sigfn? e))
      (is-static? e)))

(defn incr-time-arg [e]
  (let [[arg-base num] (-> e time-arg name (str/split #"_"))
        new-num (if-not num "1"
                        (-> num Integer/parseInt inc str))]
    (-> arg-base (str "_" new-num) symbol)))

(defn coerce-sigfn [e]
  (if (sigfn? e)
    e
    (list 'clojure.core/fn [init-time-arg] e)))

;; (defn coerce-thread-fn [e]
;;   (m/apply-if e fn? list))

;; FIXME
;; Should be equivalent to premap when post is nil, and vice versa
#_(defn sigfn-bimap [e pre post]
    (let [e (coerce-sigfn e)
          [pre post] (map coerce-thread-fn [pre post])
          new-t (incr-time-arg e)]
      `(fn [~new-t] ~(filter some? `(-> ~new-t
                                        ~pre
                                        (~e)
                                        ~post)))))
(defn sigfn-bimap [pre-f e post-f]
  (let [e (coerce-sigfn e)
        pre-valid? (s/valid? :clojure.expr/one-arg-fn pre-f)
        post-valid? (s/valid? :clojure.expr/one-arg-fn post-f)
        ;; If we're modifying time, then we'll increment
        ;; the time wrapping function's time arg so that we
        ;; can compare numbers to see if two signals are
        ;; being passed the same time or different
        new-t (if pre-valid?
                (incr-time-arg e)
                (time-arg e))
        ;; PRE
        new-e-body (if pre-valid?
                     `(u/pass-time-to (u/apply-fn-to-time ~pre-f ~new-t) ~e)
                     `(u/pass-time-to ~new-t ~e))
        ;; POST
        new-e-body (if post-valid?
                     `(u/apply-fn-to ~post-f ~new-e-body)
                     new-e-body)]
    `(fn [~new-t]
       ~new-e-body)))

(comment
  (const? (sigfn-bimap '(fn [y] (/ y 1000))
                       '(fn [t_0] (+ 1 t_0))
                       '(fn [x] (* x 2))))
  ;;
  )

(defn sigfn-premap [e f]
  (sigfn-bimap f e nil))

(defn sigfn-postmap [e f]
  (sigfn-bimap nil e f))

;; Used for when we have a single op with multiple,
;; potentially `sigfn` args
(defn sigfn-apply-postmap
  ([op args]
   (sigfn-apply-postmap init-time-arg op args))
  ([time-arg op args]
   (let [args (map #(if-not (sigfn? %)
                      %
                      `(pass-time-to ~time-arg ~%))
                   args)]
     `(fn [~time-arg]
        (~op ~@args)))))

(comment
  (sigfn-apply-postmap '+ '[1 2 (fn [t_0] (* 2 t_0))]))

;; (defn thread-first-arg [e t]
;;   `(-> ~t ~(coerce-thread-fn e)))

#_(defn apply-postmap [op fs]
    (let [new-t init-time-arg
          process-f (fn [e]
                      (if (sigfn? e)
                        `(u/pass-time-to ~new-t ~e)
                        e))
          args (map process-f fs)]
      `(fn [~new-t] (~op ~@args))))

(comment

  (clojure.core/fn
    [t_2]
    (timelines.utils/apply-fn-to
     (fn [aoeu] (/ 9999 aoeu))
     (timelines.utils/pass-time-to
      (timelines.utils/apply-fn-to-time t_2)
      (clojure.core/fn
        [t_1]
        (timelines.utils/apply-fn-to
         (fn [x] (+ x 1 2))
         (timelines.utils/pass-time-to
          (timelines.utils/apply-fn-to-time t_1 (fn [x] (* 1000 x)))
          (fn [t_0] (+ 1 t_0))))))))

;;
  )

;; (concat '(1 2) '(3))

;; TODO make these all hold
;; - Takes an operator symbol
;; - finds its arities
;; - returns an anonymous function with just as many arities
;; - each arity expects exprs (which may or may not be sigfns)
;; - and returns a sigfn that takes time and passes it to the exprs
;;     if they are sigfns
(defn op-raise-post [f-sym]
  (let []
    (fn [& args]
      (let [time init-time-arg
            args (map #(if-not (sigfn? %)
                         %
                         `(u/pass-time-to ~time ~%))
                      args)]
        `(fn [~time] (~f-sym ~@args)))))
  #_(let [arities (u/get-arities f-sym)
          ;; return `(fn ~args ~body)
          make-body (fn [arity] `(~f-sym ~@arity))
          arity-impls (for [arity arities]
                        (list arity (make-body arity)))]
      `(fn ~@arity-impls)

      #_(fn [& args]
          `(fn [~init-time-arg]
             (~f ~@args)))))

;; TODO @correctness this should increment the time arg?
(defn op-raise-pre [f-sym]
  (let [time init-time-arg]
    (fn [sigfn]
      `(fn [~time]
         (u/pass-time-to (u/apply-fn-to-time ~f-sym ~time)
                         ~sigfn)))))
