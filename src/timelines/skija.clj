(ns timelines.skija
  (:require [clojure.java.io :as io])
  (:import
   [org.jetbrains.skija
    ;; BackendRenderTarget
    ;; Canvas
    ;; ColorSpace
    ;; DirectContext
    ;; FramebufferFormat
    Paint
    Font
    Typeface
    Data
    ;; Rect
    ;; Surface
    ;; SurfaceColorFormat
    ;; SurfaceOrigin
    ]))
(def memoized-load-typeface
  (memoize (fn [path]
             (with-open [is (io/input-stream (io/resource path))]
               (let [bytes (.readAllBytes is)]
                 (with-open [data (Data/makeFromBytes bytes)]
                   (Typeface/makeFromData data)))))))
