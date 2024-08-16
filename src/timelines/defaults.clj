(ns timelines.defaults
  (:require [timelines.colors :refer :all]
            [timelines.utils :as u])
  (:import
   [io.github.humbleui.skija Paint PaintMode]))

(def default-color palette-white)
(def default-stroke-width 1)
(def default-paint-style :fill)
(def default-paint-color default-color)

(def default-paint
  (doto (Paint.)
    (.setColor (u/color default-color))
    (.setStrokeWidth  default-stroke-width)
    (.setMode (PaintMode/FILL))
    (.setAlphaf 1.0)))

