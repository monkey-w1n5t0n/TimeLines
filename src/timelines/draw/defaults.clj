(ns timelines.draw.defaults
  (:require [timelines.graphic.macros :refer [defrecord-graphic]])
  (:import
   [org.jetbrains.skija
    ;; BackendRenderTarget
    ;; Canvas
    ;; ColorSpace
    ;; DirectContext
    ;; FramebufferFormat
    Paint PaintMode
    Rect
    ;; Surface
    ;; SurfaceColorFormat
    ;; SurfaceOrigin
    ]
   )
  )


;; TODO
(def black 0xFF000000)
(def blue 0xFF0000ff)
(def grey 0xFF333333)
(def red 0xFFFF0000)
(def default-color red)

(def style-values [:paint-style/fill :paint-style/stroke :paint-style/stroke-and-fill])

(def paint-default-params {;; :anti-alias? true
                           :color default-color
                           :style (PaintMode/FILL)
                           :stroke-width 1
                           ;; :stroke-cap :paint-stroke-cap/round
                           :alpha 1.0})


(defrecord-graphic R-Paint [color alpha style stroke-width stroke-cap])

(defn make-paint
  ([] (map->R-Paint {}))
  ([color stroke-width & opts]
   (map->R-Paint
    {:color color :stroke-width stroke-width}) ))

(def default-paint (map->R-Paint paint-default-params))
