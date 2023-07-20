(ns timelines.examples
  (:require  [timelines.signal.core :refer :all]))

(def sig-1-expr '(fn [t] (+ 1 t)))
(def sig-1 (signal (fn [t] (+ 1 t))))


(def sig-2 (make-signal 3))
(def sig-3 (make-signal "hello"))
