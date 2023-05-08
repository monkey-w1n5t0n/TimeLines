(ns timelines.draw.graphics
  (:require [quil.core :as q]
            [timelines.util.macros :refer [pprint-macroexpand-1]]
            [timelines.signal.core :refer :all]
            [timelines.signal.api :refer :all]
            [timelines.protocols :refer [P-Samplable sample-at]]
            [timelines.util.core :refer :all]))

;; PROTOCOLS
(defprotocol P-Drawable
  "Draw a static object"
  (draw [x] [x &opts] "Draw a graphics object."))

(defprotocol P-Samplable-Drawable
  (draw-at [this t] "Draw a signal graphics object."))


;;;;;;;;;
;; Arc
;;;;;;;;;

(defrecord Arc [x y w h start stop])

(defn arc [x y w h start stop]
  (->Arc x y w h start stop))

(defn draw-arc [{:keys [x y w h start stop] :as arc}]
  (q/arc x y w h start stop))

(defn sample-arc-at [arc t]
  (map->Arc (update-map arc #(when % (sample-at % t)))))

(extend-type Arc
  P-Drawable
  (draw [x] (draw-arc x))

  P-Samplable
  (sample-at [this t] (sample-arc-at this t))

  P-Samplable-Drawable
  (draw-at [this t] (-> this (sample-at t) draw)))


;;;;;;;;;
;; Ellipse
;;;;;;;;;

(defrecord Ellipse [x y w h])

(defn ellipse [x y w h]
  (->Ellipse x y w h))

(defn draw-ellipse [{:keys [x y w h] :as ellipse}]
  (q/ellipse x y w h))

(defn sample-ellipse-at [ellipse t]
  (map->Ellipse (update-map ellipse #(when % (sample-at % t)))))

(extend-type Ellipse
  P-Drawable
  (draw [x] (draw-ellipse x))

  P-Samplable
  (sample-at [this t] (sample-ellipse-at this t))

  P-Samplable-Drawable
  (draw-at [this t] (-> this (sample-at t) draw)))


;;;;;;;;;
;; Line
;;;;;;;;;

(defrecord Line [p1 p2])

(defn line [p1 p2]
  (->Line p1 p2))

(defn draw-line [{:keys [p1 p2] :as line}]
  (q/line p1 p2))

(defn sample-line-at [line t]
  (map->Line (update-map line #(when % (sample-at % t)))))

(extend-type Line
  P-Drawable
  (draw [x] (draw-line x))

  P-Samplable
  (sample-at [this t] (sample-line-at this t))

  P-Samplable-Drawable
  (draw-at [this t] (-> this (sample-at t) draw)))


;;;;;;;;;
;; Quad
;;;;;;;;;

(defrecord Quad [p1 p2 p3 p4])

(defn quad [p1 p2 p3 p4]
  (->Quad p1 p2 p3 p4))

(defn draw-quad [{:keys [p1 p2 p3 p4] :as quad}]
  (apply q/quad (apply concat [p1 p2 p3 p4])))

(defn sample-quad-at [quad t]
  (map->Quad (update-map quad #(when % (sample-at % t)))))

(extend-type Quad
  P-Drawable
  (draw [x] (draw-quad x))

  P-Samplable
  (sample-at [this t] (sample-quad-at this t))

  P-Samplable-Drawable
  (draw-at [this t] (-> this (sample-at t) draw)))

;;;;;;;;;
;; RECT
;;;;;;;;;
;; TODO @completeness add (optional) radius parameters
(defrecord Rect [x y w h])

;; TODO use other parameters?
(defn rect [x y w h]
  (->Rect x y w h))

(defn draw-rect [{:keys [x y w h] :as rect}]
  (q/rect x y w h))

(defn sample-rect-at [rect t]
  (map->Rect (update-map rect #(when % (sample-at % t)))))

(extend-type Rect
  P-Drawable
  (draw [x] (draw-rect x))

  P-Samplable
  (sample-at [this t] (sample-rect-at this t))

  P-Samplable-Drawable
  (draw-at [this t] (-> this (sample-at t) draw)))


;;;;;;;;;
;; Triangle
;;;;;;;;;

(defrecord Triangle [p1 p2 p3])

(defn triangle [p1 p2 p3]
  (->Triangle p1 p2 p3))

(defn draw-triangle [{:keys [p1 p2 p3] :as triangle}]
  (apply q/triangle (apply concat [p1 p2 p3])))

(defn sample-triangle-at [triangle t]
  (map->Triangle (update-map triangle #(when % (sample-at % t)))))

(extend-type Triangle
  P-Drawable
  (draw [x] (draw-triangle x))

  P-Samplable
  (sample-at [this t] (sample-triangle-at this t))

  P-Samplable-Drawable
  (draw-at [this t] (-> this (sample-at t) draw)))






(comment
  ;; TODO @dbg
  (defmacro defrecord-shape [record-name keys draw-fn]
    (let [record-sym (symbol (clojure.string/lower-case (name record-name)))
          draw-name (symbol (str "draw-" (name record-name)))
          sample-name (symbol (str "sample-" (name record-name) "-at"))
          record-constructor (symbol (str "->" (name record-name)))]
      `(do
         (defrecord ~record-name ~keys)
         (defn ~record-sym ~keys (~record-constructor ~@keys))
         (defn ~draw-name [{:keys ~keys :as shape}] ~draw-fn)
         (defn ~sample-name [shape t] (map->~record-name (update-map shape #(when % (sample-at % t)))))
         (extend-type ~record-name
           P-Drawable
           (draw [x] (~draw-name x))

           P-Samplable
           (sample-at [this t] (~sample-name this t))

           P-Samplable-Drawable
           (draw-at [this t] (-> this (sample-at t) draw))))))


  (pprint-macroexpand-1
   (defrecord-shape Rect [x y w h] (q/rect x y w h))))
