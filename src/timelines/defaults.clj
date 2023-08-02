(ns timelines.defaults
  (:require [timelines.colors :refer :all]
            [timelines.utils :as u])
  (:import
   [org.jetbrains.skija Paint PaintMode]))

(def default-color palette-white)
(def default-stroke-width 1)
(def default-paint-style (PaintMode/FILL))
(def default-paint-color default-color)

(def default-paint
  (doto (org.jetbrains.skija.Paint.)
    (.setColor (u/color default-color))
    (.setStrokeWidth  default-stroke-width)
    (.setMode  default-paint-style)
    (.setAlphaf 1.0)))
