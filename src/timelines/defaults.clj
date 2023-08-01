(ns timelines.defaults
  (:require [timelines.colors :refer :all])
  (:import
   [org.jetbrains.skija Paint PaintMode]))

(def default-color palette-white)
(def default-stroke-width 1)
(def default-paint-style (PaintMode/FILL))
(def default-paint-color default-color)
