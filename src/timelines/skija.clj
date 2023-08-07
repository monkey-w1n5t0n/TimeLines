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
#_(def memoized-load-typeface
    (memoize (fn [path]
               (with-open [is (io/input-stream (io/resource path))]
                 (let [bytes (.readAllBytes is)]
                   (with-open [data (Data/makeFromBytes bytes)]
                     (Typeface/makeFromData data)))))))
(defn load-typeface [path]
  (with-open [is (io/input-stream (io/resource path))]
    (let [bytes (.readAllBytes is)]
      (with-open [data (Data/makeFromBytes bytes)]
        (Typeface/makeFromData data)))))

(defonce *font-cache (atom {}))

(def font-name->resource-path
  {"FiraCode Regular" "fonts/FiraCode-Regular.ttf"})

(defn load-fonts []
  (doseq [[font path] font-name->resource-path]
    (when-not (contains? @*font-cache font)
      (swap! *font-cache assoc font (load-typeface path)))))

(defn init []
  (load-fonts))
