(ns timelines.api
  (:require
   [timelines.base-api :as base]
   [timelines.signal :as sig]
   [timelines.expr :as e]
   [timelines.utils :as util]
   [timelines.macros :refer [with-ns <- <<- <-as]] ;; ->
   [timelines.protocols :refer :all]
   [clojure.pprint :refer [pprint]]))

(def t (sig/make-signal e/id-sigfn))

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
  (defn fast [amt sig]
    (sig/sig-premap `(fn [~'t] (clojure.core/* ~'t ~amt))
                    sig))

  (comment
    (fast 2 t))

  (defn slow [amt sig]
    (sig/sig-premap `(fn [~'t] (clojure.core// ~'t ~amt))
                    sig))

;;;; Postmaps
  (def + (sig/op-raise-post 'clojure.core/+))
  (def - (sig/op-raise-post 'clojure.core/-))
  (def * (sig/op-raise-post 'clojure.core/*))
  (def / (sig/op-raise-post 'clojure.core//))
  (def mod (sig/op-raise-post 'clojure.core/mod))
  (def % mod)
  (defn mod1 [sig] (mod sig 1.0))
  (def %1 mod1)
  (def sine (sig/op-raise-post 'Math/sin))
  (def sin sine)
  (def nth (sig/op-raise-post 'clojure.core/nth))
  (def from-list (sig/op-raise-post 'timelines.base-api/from-list))
  (def fromList from-list)
  (def str (sig/op-raise-post 'clojure.core/str))
  (def int (sig/op-raise-post 'clojure.core/int))
  (def min (sig/op-raise-post 'clojure.core/min))
  (def max (sig/op-raise-post 'clojure.core/max))
  (def abs (sig/op-raise-post 'clojure.core/abs))
  ;; ;; TODO @properness there's got to be a better way...
  ;; ;; can't find out how to properly resolve Math/sin
  ;; (defn sine-impl [x]
  ;;   (Math/sin x))
  )

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

(defn sine01 [x]
  (+ 0.5 (* 0.5 (sine x))))

(defn scale
  [min max sig]
  (+ min (* sig (- max min))))

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
