(ns timelines.api-test
  (:require  [clojure.test :as t]
             [timelines.draw.graphics :refer [make-paint paint rect color]]
             [timelines.signal.api :refer :all]
             [timelines.protocols :refer [draw]]
             ))

(defn draw-screen []
  (let [r (rect 80 50 20 50)]
    (draw r
     #_(color r 0xFFCC3333))))
