(ns timelines.draw.text.typeface
  (:require
    [clojure.java.io :as io])
  (:import
   [java.io Writer]
   [org.jetbrains.skija
    Font
    ;; BackendRenderTarget
    ;; Canvas
    ;; ColorSpace
    ;; DirectContext
    ;; FramebufferFormat
    Paint PaintMode
    ;; Rect
    ;; Surface
    ;; SurfaceColorFormat
    ;; SurfaceOrigin
    ]

   #_[io.github.humbleui.skija Data Typeface]))

(def default-font
  (Font.))

;; (defn make-from-path
;;   (^Typeface [^String path]
;;    (Typeface/makeFromFile path 0))
;;   (^Typeface [^String path index]
;;    (Typeface/makeFromFile path index)))


(comment
  (def path "./resources/fonts/FiraCode-Regular.ttf")

  )
