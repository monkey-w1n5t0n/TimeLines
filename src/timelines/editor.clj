(ns timelines.editor
  (:require
   [quil.core :as q]
   [timelines.draw.core :refer :all]
   [timelines.util.macros :refer :all]
   ;; [timelines.parameters :refer :all]
   ;; ;; [timelines.editor :refer :all]
   ;; [timelines.utils :refer :all]
   ;; [timelines.state :refer :all]
   ))

;; (defn draw-windows []
;;   (let []
;;     (doseq [win (windows)]
;;       (let [
;;             x (+ (window-x win) (half window-padding))
;;             y (+ ( window-y  win ) (half window-padding))
;;             w (- ( window-w  win ) (half window-padding))
;;             h (- (window-h win) (half window-padding))
;;             ]
;;         (color :background)
;;         (q/rect x y w h)

;;         ))))

;; (defn draw-background []
;;   ;; (color :background)
;;   (q/fill 0)
;;   (q/rect-mode :corner)
;;   (q/rect 0 0 window-width window-height))

;; (defn draw-infoline []
;;   (let [x 0
;;         y (- window-height infoline-height)]
;;     (q/fill 100)
;;     (q/no-stroke)
;;     (q/rect x y infoline-width infoline-height)

;;     ))

(defprotocol Drawable
  (draw [this]))

;; (extend-protocol Drawable
;;   )

(def default-fill [200 200 100])
(def default-stroke 10)

(defmacro draw-with-settings [obj]
  (let [[fill stroke] (map #(get-in obj [:paint %]) [:fill :stroke])]
    `(q/with-fill ~fill
       (q/with-stroke ~stroke
         ~obj))))

(macroexpand-1 '(draw-with-settings
                 (q/rect 10 10 20 20)))

(defrecord Rectangle [x y w h]
  Drawable
  (draw [this]
    ;; (draw-with-settings (this :settings))
    (q/rect x y w h)))

(defn rect [x y w h]
  (Rectangle. x y w h))

(Rectangle. 0 0 10 10)

(def window-settings
  (letmap width 100
          height 100))

(defn fill [col item]
  (assoc-in item [:paint :fill] col))

(defn stroke [option item]
  (assoc-in item [:paint :stroke] option))

(def background
  (->>
   (rect 0 0
         (window-settings :width)
         (window-settings :height))
   (fill [144 244 100])
   (stroke :no)))

(def infobar (rect 50 50 125 321))

(defn draw-editor [w h]
  (draw background)
  (draw infobar)
  ()
  (draw (rect 0 0 w h)))
