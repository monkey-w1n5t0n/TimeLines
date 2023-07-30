(ns timelines.meta
  (:require [quil.core :as q]
            [clojure.test :refer [function?]]))

(defn empty-draw []
  (q/background 0 255 0)
  (q/stroke 255)
  (q/text-size 50)
  (q/text "EMPTY DRAW" 100 100))

(defonce *current-draw (atom empty-draw))

(defmacro set-draw!
  "Set the current draw function.
  Expects either symbol that refers to a no-args function
  or a body of code to execut."
  [& body]
  (if (and (= 1 (count body))
           (function? (first body)))
    `(reset! *current-draw ~(first body))
    `(reset! *current-draw (fn [] ~@body))))

(defn reset-draw! [] (set-draw! empty-draw))
