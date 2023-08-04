(ns timelines.defaults
  (:require [timelines.colors :refer :all]
            [timelines.utils :as u])
  (:import
   [org.jetbrains.skija Paint PaintMode]))

(defn paint-style->skija [style]
  (case style
    :fill (PaintMode/FILL)
    :stroke (PaintMode/STROKE)
    :else (throw (Exception. (str "Wrong style: " style)))))

(def default-color palette-white)
(def default-stroke-width 1)
(def default-paint-style :fill)
(def default-paint-color default-color)

(def default-paint
  (doto (org.jetbrains.skija.Paint.)
    (.setColor (u/color default-color))
    (.setStrokeWidth  default-stroke-width)
    (.setMode (paint-style->skija default-paint-style))
    (.setAlphaf 1.0)))

