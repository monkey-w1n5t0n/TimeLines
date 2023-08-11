(ns timelines.defaults
  (:require [timelines.colors :refer :all]
            [timelines.utils :as u]))

(def default-color black)
(def default-text-color palette-white)
(def default-stroke-width 1)
(def default-paint-style :fill)
(def default-paint-color default-color)

(def default-font-name "FiraCode Regular")
(def default-font-size 25)

#_(def default-paint
    (doto (org.jetbrains.skija.Paint.)
      (.setColor (u/color default-color))
      (.setStrokeWidth  default-stroke-width)
      (.setMode (PaintMode/FILL))
      (.setAlphaf 1.0)))
