(ns timelines.signal.api
  (:require
   [timelines.signal.base-api :as base]
   [timelines.signal.core :refer :all]
   [timelines.expr.core :as expr]
   [timelines.util.core :as util]
   [timelines.util.macros :refer [with-ns -> <- <<- <-as]]
   [timelines.protocols :refer :all]
   [clojure.pprint :refer [pprint]]))

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
    (apply-premap sig `(fn [time#] (clojure.core// time# ~amt))))

  (defn fast [amt sig]
    (apply-premap sig `(fn [time#] (clojure.core/* ~amt time#))))
  ;;

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

  (defn sine [& args]
    (apply-postmap {:sym 'Math/sin} args))

  (def sin sine)

  (defn nth [& args]
    (apply-postmap {:sym 'nth :source-ns 'clojure.core} args))

  (defn from-list [& args]
    (apply-postmap {:sym 'from-list
                    :source-ns 'timelines.signal.base-api}
                   args))

  (defn str [& args]
    (apply-postmap {:sym 'str :source-ns 'clojure.core} args))

  )



(comment
  (-> 1
      (+ (* 2 t))
      (- 100)
      (sample-at 4))
  )


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
