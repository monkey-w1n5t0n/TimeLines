(ns timelines.api-test
  (:require  [clojure.test :as t]
             [timelines.draw.graphics :refer [make-paint paint rect color]]
             [timelines.signal.api :refer :all]
             [timelines.protocols :refer [draw]]
             ))

(-> (rect 80 50 20 50) :paint)
(color r 0xFFCC3333)

(defn draw-screen []
  (let [r (rect 80 50 20 50)]
    (draw
     (paint r
            (make-paint
             (r 0xFFCC3333)
             3)))))
