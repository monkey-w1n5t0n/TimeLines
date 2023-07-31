(ns timelines.graphics
  (:require
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [timelines.utils :as u]
   [timelines.macros :refer [defs letmap pprint-macroexpand-1 defgraphics]]
   [timelines.time :refer [now]]
   [timelines.globals :refer [*global-canvas]]
   [timelines.protocols :refer [P-Samplable P-Drawable sample-at draw-at]] ; P-Samplable+Drawable
   ;; [timelines.draw.defaults :refer :all]
   [timelines.macros :as macro]
   [timelines.specs :as specs]
   [clojure.spec.alpha :as s]
   #_[timelines.draw.macros :refer [defrecord-graphic]])
  (:import
   [org.jetbrains.skija
    ;; BackendRenderTarget
    ;; Canvas
    ;; ColorSpace
    ;; DirectContext
    ;; FramebufferFormat
    Path
    ;; Paint
    PaintMode
    ;; Rect RRect
    Data
    Typeface Font
    ;; Surface
    ;; SurfaceColorFormat
    ;; SurfaceOrigin
    ]))

(comment
  (def test-call
    '(rect [x y w h r paint]
           P-Skijable
           (->skija [this]
                    (if r
                      (RRect/makeXYWH x y w h r)
                      (Rect/makeXYWH x y w h)))

           P-Drawable
           (draw [this] ((if  r .drawRRect .drawRect) canvas this paint)))))

(defs
  hello 1
  bye (+ hello 2))

(defgraphics Rect [x y w h r paint]
  P-Skijable
  (->skija [this]
           (if r
             (org.jetbrains.skija.RRect/makeXYWH x y w h r)
             (org.jetbrains.skija.Rect/makeXYWH  x y w h)))

  P-Drawable
  (draw [{:keys [r paint] :as rect}]
        (let [skija-rect (->skija rect)
              paint (->skija paint)
              method (if (and r (> r 0)) .drawRRect .drawRect)]
          (method @timelines.globals/*canvas skija-rect paint))))

(defn paint->skija
  "Make an actual SKPaint Java object from a Clojure R-Paint record"
  [{:keys [color style stroke-width alpha] :as paint}]
  (doto (Paint.)
    (.setColor (u/color (or color (paint-default-params :color))))
    (.setStrokeWidth (or stroke-width (paint-default-params :stroke-width)))
    (.setMode #_({:stroke (PaintMode/STROKE)
                  :fill  (PaintMode/FILL)})
     (or style (paint-default-params :style)))
    (.setAlphaf (or alpha (paint-default-params :alpha)))))

(defn font->skija [{:keys [name size path] :as f}]
  (let [typeface (with-open [is (io/input-stream (io/resource path))]
                   (let [bytes (.readAllBytes is)]
                     (with-open [data (Data/makeFromBytes bytes)]
                       (Typeface/makeFromData data 0))))]
    (Font. typeface size)))

(defn rect->skija [{:keys [x y w h r] :as rect}]
  (if (and r (> r 0))
    (RRect/makeXYWH x y w h r)
    (Rect/makeXYWH x y w h)))

(defn oval->skija [{:keys [x y w h] :as oval}]
  (Rect/makeXYWH x y w h))

(defn draw-circle
  ([circle] (draw-circle circle @*global-canvas))
  ([{:keys [x y r fill stroke]} canvas]
   (.DrawCircle canvas x y r fill)
   (when stroke
     (.DrawCircle canvas x y r stroke))))

(defn draw-rect
  ([rect] (draw-rect rect @*global-canvas))
  ([{:keys [x y w h r paint] rect} canvas]
   (let [skija-paint (or (:skija paint)
                         (paint->skija paint))
         skyja-rect (or (:skija r))]
     (if r
       (.drawRRect canvas)))))

(defn circle->skija [])

;; MACROS
(do)

;; Paints
(do
  ;; TODO
  (def white 0xFFFFFFFF)
  (def black 0xFF000000)
  (def blue 0xFF0000ff)
  (def grey 0xFF333333)
  (def red 0xFFFF0000)
  (def default-color red)

  (def palette-red        0xFFE63946)
  (def palette-white      0xFFA8DADC)
  (def palette-blue-light 0xFFA8DADC)
  (def palette-blue-medium     0xFF457B9D)
  (def palette-blue-dark       0xFF1D3557)

  (def default-color palette-white)

  (def style-values [:paint-style/fill :paint-style/stroke :paint-style/stroke-and-fill])

  (def paint-default-params {;; :anti-alias? true
                             :color default-color
                             :style (PaintMode/FILL)
                             :stroke-width 1
                             ;; :stroke-cap :paint-stroke-cap/round
                             :alpha 1.0})

  (defrecord-graphic Paint [color alpha style stroke-width stroke-cap])

  (defgraphics Paint [color style])

  (def default-stroke-width 1)

  (defn paint-style
    "Style keyword must be qualified, e.g. :paint-style/fill"
    [paint s]
    (assoc paint :style
           (case s
             :fill (PaintMode/FILL)
             :stroke (PaintMode/STROKE)
             :else (paint-default-params :style))))

  (defn paint-color [paint col]
    (assoc paint :color col))

  (defn paint-alpha [paint a]
    (assoc paint :alpha a))

  (defn paint-stroke-width
    [paint sw]
    (assoc paint :stroke-width sw))

  (defn paint-stroke-cap
    "Stroke cap keyword must be qualified, e.g. :paint-stroke-cap/round"
    [paint sc]
    (assoc paint :stroke-cap sc))

  (defn make-paint
    ([] (map->R-Paint {}))
    ([color]
     (map->R-Paint {:color color}))
    ([color style]
     (->
      (map->R-Paint {:color color})
      (paint-style style)))
    ([color style stroke-width]
     (-> (map->R-Paint {:color color})
         (paint-style style)
         (paint-stroke-width stroke-width)))
    ([color style stroke-width & opts]
     (map->R-Paint
      {:color color :opts opts})))

  (def default-paint (map->R-Paint paint-default-params))

  (def test-paint (-> (make-paint)
                      (paint-color 0xFF333333)
                      (paint-stroke-width 2)
                      (paint-style :stroke)
                      R-Paint->SKPaint))

  (defn paint [obj p]
    (assoc obj :paint p))

  (defn color [obj c]
    (update obj :paint (paint-color c))))

(comment

  (macro/pprint (defrecord-graphic R-Paint [color alpha style stroke-width stroke-cap])))

;; Rects
(do

  (defrecord-graphic R-Rect [x y w h r])

  (defn rect
    ([x y w h]
     (map->R-Rect {:x x :y y :w w :h h}))
    ([x y w h r]
     (map->R-Rect {:x x :y y :w w :h h :r r}))
    ;; ([x y w h r1 r2]
    ;;  (map->R-Rect {:x x :y y :w w :h h :r1 r1 :r2 r2}))
    )

  (defn radius [obj r]
    (assoc obj :r r)
    #_([obj r1 r2]
       (assoc obj :r1 r1 :r2 r2)))

  (comment
    (def test-rect (rect 50 50 80 60)))

  (extend-type R-Rect
    P-Drawable
    (draw [{:keys [x y w h paint r]}]
      (let [paint (R-Paint->SKPaint
                   (or paint default-paint))
            rounded? r]
        (if rounded?
          ;; Rounded
          (.drawRRect @*global-canvas
                      (RRect/makeXYWH x y w h r)
                      paint)
          ;; Regular
          (.drawRect @*global-canvas
                     (Rect/makeXYWH x y w h)
                     paint))))))

;; Text
(do

  (def default-font (load-font "fonts/FiraCode-Regular.ttf"))

  (def default-text-paint (make-paint palette-white))

  (def default-text-size 20)

  (defn font-size [text s]
    (assoc text :size s))

  (defgraphic Text [text x y size paint font]
    P-Skijable
    (->skija [this]))

  (defn make-text
    ([t x y]       (Text t x y   default-text-size default-text-paint default-font))
    ([t x y s]     (Text t x y s                   default-text-paint default-font))
    ([t x y s p]   (Text t x y s p                                    default-font))
    ([t x y s p f] (Text t x y s p f)))

  (def text make-text)

  (extend-type R-Text
    P-Drawable
    (draw [{:keys [text x y paint font size]}]
      (let [paint (R-Paint->SKPaint paint)]
        (.drawString @*global-canvas text (float x) (float y) (.setSize font size) paint)))))

(do

  (extend-protocol P-Samplable
    org.jetbrains.skija.PaintMode
    (sample-at [this _]
      this)

    org.jetbrains.skija.Font
    (sample-at [this _]
      this)))
