(ns timelines.api
  (:require
   ;; [timelines.maths :as maths]
   [timelines.signal.core :refer :all]
   [timelines.util.core :as util]
   [timelines.protocols :refer :all]
   #_(
      [timelines.expression :as expr :refer :all]
      [clojure.pprint :refer [pprint]]
      [timelines.types :refer :all]
      [timelines.signal :as signal])
   ))





;; (defmacro make-sig-op [sym]
;;   `(fn [& args#]
;;      (signal (cons '~sym (map graph args#)))))

;; TODO add support for specific arities other than variadic?
;; TODO @completeness do better checking for whether the new
;; signal will be const or not
(defmacro defop [sym]
  `(defn ~(util/strip-symbol sym) [& args#]
     (let [exprs# (map ->expr args#)
           new-expr# (cons '~sym exprs#)
           const?# (all-const-sigs? args#)
           ]
       (-> (make-signal new-expr#)
           (assoc :const? const?#)))))



(comment
  (defop +)
  (+ 1 2 t)

  (pprint (macroexpand-1 '(defop +)))
  (sample-at (+ t 1) 1)

  )










;;;;;;;;;;;;;
;; Old
;;;;;;;;;;;;;

;; (deftype Expression-Divider [])

;; (def | (Expression-Divider.))
;; (def |> (Expression-Divider.))
;; (def <| (Expression-Divider.))

;; ;; (declare m-to-f fromList slow fast sqr saw sin sine cos cosine wrap01 pi)

;; (do
;;   (defmacro expr [& body]
;;     `(quote (~@body)))

;;   (expr
;;    m-to-f | fromList [1 2 3 4]
;;    | slow 2
;;    | sqr
;;    | * 2 pi t))

;; ;; (some #(instance? Signal %)
;; ;;       [1 2 3 4])


;; ;; Arithmetic Ops
;; (def sig-func-map
;;   {'+   sig-add
;;    '-   sig-sub
;;    '*   sig-mul
;;    '/   sig-div
;;    '%   sig-mod
;;    'sin sig-sin})

;; ;; (defmacro def-arithmetic-ops []
;; ;;   (let [defs (for [[sym f] (seq sig-func-map)]
;; ;;                `(def ~sym ~f))]
;; ;;     `(do ~@defs)))








;; (defn lerp [from to phasor]
;;   (let [range (- to from)]
;;     (+ from (* range phasor))))


;; (defn bi-to-uni [s]
;;   )

;; (defn sine-bi [x]
;;   (maths/sine x))

;; (defn sine-bi [x]
;;   (maths/sine x))
