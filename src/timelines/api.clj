(ns timelines.api
  (:require
   [timelines.base-api :as base]
   [timelines.signal :refer [make-signal var-sig? apply-premap apply-postmap]]
   [timelines.expr :as expr]
   [timelines.utils :as util]
   [timelines.macros :refer [with-ns <- <<- <-as]] ;; ->
   [timelines.protocols :refer :all]
   [clojure.pprint :refer [pprint]]))

(def t (make-signal '(fn [time_0] time_0)))

(defmacro ->map
  [args]
  `(hash-map ~@(mapcat #(vector (keyword (name %))
                                %)
                       args)))

;; TODO this isn't being used yet
(defrecord SigOp [sym kind source-ns])

;; OPERATORS
(do ;; TODO abstract this?
;;;; Premaps
  (defn slow [amt sig]
    (if (var-sig? amt)
      ;; Signal amt
      (let [sig-fn-expr (:expr amt)]
        (apply-premap sig `(fn [time#] (clojure.core// time#
                                                       (~sig-fn-expr time#)))))
      ;; Static amt
      (apply-premap sig `(fn [time#] (clojure.core// time# ~amt)))))

  (defn fast [amt sig]
    (if (var-sig? amt)
      (let [sig-fn-expr (:expr amt)]
        (apply-premap sig `(fn [time#] (clojure.core/* time#
                                                       (~sig-fn-expr time#)))))
      (apply-premap sig `(fn [time#] (clojure.core/* ~amt time#)))))
  ;;

  ;; TODO
  (def fns '[+ - * / mod])

;;;; Postmaps
  (defn + [& args]
    (apply-postmap {:sym '+ :source-ns 'clojure.core} args))

  (defn - [& args]
    (apply-postmap {:sym '- :source-ns 'clojure.core} args))

  (defn * [& args]
    (apply-postmap {:sym '* :source-ns 'clojure.core} args))

  (defn / [& args]
    (apply-postmap {:sym '/ :source-ns 'clojure.core} args))

  (defn mod [& args]
    (apply-postmap {:sym 'mod :source-ns 'clojure.core} args))

  (def % mod)

  (defn mod1 [sig]
    (mod sig 1.0))

  (def %1 mod1)

  ;; TODO @properness there's got to be a better way...
  ;; can't find out how to properly resolve Math/sin
  (defn sine-impl [x]
    (Math/sin x))

  (defn sine [& args]
    (apply-postmap {:sym 'sine-impl} args))

  (def sin sine)

  (defn nth [& args]
    (apply-postmap {:sym 'nth :source-ns 'clojure.core} args))

  (defn from-list [& args]
    (apply-postmap {:sym 'from-list
                    :source-ns 'timelines.base-api}
                   args))

  (defn str [& args]
    (apply-postmap {:sym 'str :source-ns 'clojure.core} args))

  (defn int [& args]
    (apply-postmap {:sym 'int :source-ns 'clojure.core} args)))

;; convenience arithmetic
(do
  (def pi Math/PI)

  (def twoPi (* 2 pi))

  (defn half [x] (/ x 2))
  (defn third [x] (/ x 3))
  (defn quarter [x] (/ x 4))
  (defn eighth [x] (/ x 8))

  (defn double [x] (* x 2))
  (defn triple [x] (* x 3)))

(comment
  (+ 1 t))

;; Misc API functions
(do
  (defn sine01 [x]
    (+ 0.5 (* 0.5 (sine x))))

  (defn scale
    [min max sig]
    (+ min (* sig (- max min)))))

;; TODO register ops so that we can introspect
(comment
  (def *sig-ops (atom {}))

  (defn register-sig-op [sym {:keys [kind source-ns]}]
    (let [op-already-registered? (sym @*sig-ops)]
      (if (not op-already-registered?)
        (swap! *sig-ops assoc sym
               (map->SigOp
                (->map [sym kind source-ns])))
        (throw (RuntimeException. (str "Attempt at redifining signal operator " sym "."))))))
  ;; ;; Convenience macros for defining
  ;; (defn def-sig-op [sym kind]
  ;;   (let [op-maker (kind {:postmap #'postmap-op
  ;;                         :premap  #'premap-op})
  ;;         source-ns (sym->ns sym)]
  ;;     (register-sig-op ~sym (->map [kind sym source-ns]))
  ;;     (def sym (op-maker (ns-resolve 'clojure.core ~sym)))))
  )
