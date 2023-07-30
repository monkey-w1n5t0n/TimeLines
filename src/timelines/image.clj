(ns timelines.image
  (:require [quil.core :as q]))


;; Wrappers
(defn create-graphics [w h]
  (q/create-graphics w h :p2d))

(defn create-image [w h]
  {:w w :h h :img (q/create-image w h :argb)})

(defn image-dimensions [img]
  [(:w img) (:h img)])

(defn fill-image [img c]
  (let [[w h] (image-dimensions img)]
    (dotimes [x w]
      (dotimes [y h]
        (q/set-pixel (:img img) x y c)))
    img))

(defn draw-image [img, x, y]
  (q/image (:img img) x y))

(defn fill-graphics [image]
  )

(defn create-fill-image [w h c]
  ())

(defn fill-graphics [image]
  )

(defn create-fill-image [w h c]
  ())

;;;;;;;
(defn fill-buffer [buf col]
  (fill-image buf col))

(defn create-buffer [w h]
  (create-image w h))

(defn initialise-buffers [num w h]
  (when-not (-> *state :graphics-buffers)
    (send *state assoc :graphics-buffers []))
  (dotimes [i num]
    (let [buffer (create-buffer w h)]
      (fill-buffer buffer (q/color 0 0 0 0))
      (send *state update :graphics-buffers #(conj % i)))))
