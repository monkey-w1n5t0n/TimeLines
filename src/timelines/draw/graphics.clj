(ns timelines.graphics
  (:require [quil.core :as q]
            [timelines.signal :refer :all]
            [timelines.utils :refer :all]))

;; PROTOCOLS
(defprotocol IRenderable
  "Render a static object"
  (render [x] [x &opts] "Render a graphics object."))

(defprotocol IRenderable-Signal
  "Render a signal object at a specific moment in time"
  (render-at [x t] "Draw a time-varying graphics object."))


(defn draw-rect [{:keys [x y w h] :as rect}]
  (q/rect x y w h))


(defrecord Test [x y w h])



(defn signal-rect [& params]
  (Signal. (fn [t]
             (Rect. ))))


;;;;;;;;;
;; RECT
;;;;;;;;;
(defrecord Rect [x y w h col radius]
  Drawable
  (draw [x] (draw-rect x)))

(defn sample-rect-at [rect t]
  (map->Rect (update-map rect #(sample-sig-at % t))))

(defrecord Signal-Rect [x y w h]
  Time-Variable
  (sample-at [rect t] (sample-rect-at rect t))

  Time-Drawable
  (draw-at [x t] (draw (sample-sig-at x t)))
  )

(defn xywh [x y w h]
  {:x x :y y :w w :h h})


;;;;;;;
;; TEST
;;;;;;;
