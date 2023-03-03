(ns timelines.namespace
  (:require
   ;; [clojure.math :as math]
   [timelines.signal :as sig]))


(defn is-var-of-fn? [v]
  (if (var? v)
    (fn? (var-get v))
    false))

;; (defn create-and-populate-ns []
;;   (create-ns 'timelines-UI)
;;   (doseq
;;       [[k v] (seq (ns-map
;;                    (find-ns 'clojure.core)))]
;;       (println (str k " -> " v ", fn: " (is-var-of-fn? v) ", type: " (type v))))
;;   )

;;;; Unary operators
;; Numeric
(sig/defop sqrt)
(sig/defop cbrt)
(sig/defop pow)
(sig/defop exp)
(sig/defop log)
(sig/defop round)
(sig/defop floor)
(sig/defop ceil)
;; Trigonometric
(sig/defop sin)
(sig/defop asin)
(sig/defop sinh)

(sig/defop cos)
(sig/defop acos)
(sig/defop cosh)

(sig/defop tan)
(sig/defop atan)
(sig/defop tanh)

;;;; Binary Operators
(sig/defop %)

;;;; Variadic Operators
(sig/defop +)
(sig/defop -)
(sig/defop *)
(sig/defop /)


(def asig (timelines.signal/Signal. (+ 1 2 sig/t)))
