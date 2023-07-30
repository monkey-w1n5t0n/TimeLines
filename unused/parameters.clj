(ns timelines.parameters
  (:require [timelines.utils :refer :all]
            [timelines.macros :refer :all]
            [quil.core :as q]))
(defs
  window-width 600
  window-height 400)

(defs
  ascender-relative-offset -0.305
  capital-relative-offset -0.231
  median-relative-offset -0.07
  baseline-relative-offset 0.48
  descender-relative-offset 0.68)

(defs
  color-map {:background [29 31 33]
             :point [255 107 107]
             :text [177 180 179]}

  window-padding 10

  effective-height (- window-height (* 2 window-padding))
  effective-width (- window-width (* 2 window-padding))

  buffer-left window-padding
  buffer-right (- window-width window-padding)
  buffer-top window-padding
  buffer-bot (- window-height window-padding)

  a 30
  row-height a
  row-width effective-width

  col-width a
  col-height effective-height

  num-rows (/ effective-height row-height)
  num-cols (/ effective-width col-width)
  )

(defs
  infoline-height 20
  infoline-width window-width
  window-padding 2
  )


(defn color [kwd]
  (let [[r g b] (kwd color-map)]
    (q/fill r g b)))
