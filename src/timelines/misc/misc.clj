(ns timelines.misc
  (:require [quil.core :as q]
            [timelines.parameters :refer :all]

            ))
(defn draw-grid []
  (let [num-cells (* num-rows num-cols)
        a 16]
    (doseq [r (range num-rows)
            c (range num-cols)]
      (let [x (col-x c)
            y (row-y r)]
        (q/fill 255 200 200)
        (q/rect-mode :center)
        (q/rect x y a a)
        ;; (q/line [a])
        ))))

(defn sketch []
  (q/background 100 200 200))


(defmacro test [arg]
  `(do
     (println (str "Hi, type: " (type ~arg)))
     (+ ~arg 1)))



(eval (eval  (clojure.core/read-string "`(let [x# 1]
   (+ 1 x#))")))
