(ns timelines.editor
  (:require
   [timelines.colors :refer :all]
   [clojure.stacktrace :as stacktrace]
   [timelines.graphics :refer :all]
   [timelines.api :refer :all]
   [timelines.protocols :refer [sample-at draw-now]]
   [timelines.globals :refer [screen-height screen-width]]
   [timelines.time :refer [now]]
   [timelines.macros :refer [pprint]]))

(defonce *broken? (atom false))
(defonce *stacktrace-printed? (atom false))

(defn scene []
  (rect 0 200 300 300 :paint (paint blue)))

(def broken-scene (rect 0 0 screen-width screen-height :paint
                        (paint red)))

(defn draw-fps [n]
  (draw-now (text (str "FPS: " (int n)) 3
                  (- screen-height 3)
                  (font (scale 20 25 (sine01 t))))))

(defn draw-screen []
  (try
    (do
      (draw-now (scene))
      (reset! *broken? false)
      (reset! *stacktrace-printed? false))
    (catch Exception e
      (reset! *broken? true)
      (draw-now broken-scene)
      (when-not @*stacktrace-printed?
        (stacktrace/print-stack-trace (stacktrace/root-cause e))
        (reset! *stacktrace-printed? true)))))
