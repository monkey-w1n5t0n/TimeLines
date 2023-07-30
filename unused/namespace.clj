(ns timelines.misc.namespace
  (:require
   ;; [clojure.math :as math]
   [timelines.signal :as sig]))

(comment
  (defn all-defined-symbols []
    (apply concat
           (for [ns (all-ns)]
             (map (fn [sym] (symbol (str (ns-name ns)) (str sym)))
                  (keys (ns-publics ns))))))

  (->> (all-defined-symbols)
       (drop 380)
       (take 10)
       (filter #()))

  (defn all-defined-functions []
    (apply concat
           (for [ns (all-ns)]
             (map (fn [sym] (symbol (str (ns-name ns)) (str sym)))
                  (filter #(fn? (ns-resolve ns %)) (keys (ns-publics ns)))))))


  (defn all-defined-functions []
    (apply concat
           (for [ns (all-ns)]
             (let [publics (ns-publics ns)]
               (map (fn [sym] (symbol (str (ns-name ns)) (str sym)))
                    (filter #(fn? (get publics %)) (keys publics)))))))

  (take 20 (all-defined-functions))
)





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
