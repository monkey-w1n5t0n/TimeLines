(ns timelines.draw.text.typeface
  (:require
   [clojure.java.io :as io]
   [timelines.globals :refer [*global-canvas]])
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


(defn draw-string [{:keys [string x y]}]
  (.drawString @*global-canvas string x y default-font))

(draw-string {:string "Hello" 10 200})
;; (defn make-from-path
;;   (^Typeface [^String path]
;;    (Typeface/makeFromFile path 0))
;;   (^Typeface [^String path index]
;;    (Typeface/makeFromFile path index)))


(comment
  (def path "./resources/fonts/FiraCode-Regular.ttf")

  )
