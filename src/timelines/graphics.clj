(ns timelines.graphics
  (:require
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [timelines.utils :as u]
   [timelines.macros :refer [letmap pprint-macroexpand-1]]
   [timelines.time :refer [now]]
   [timelines.globals :refer [*global-canvas]]
   [timelines.protocols :refer [P-Samplable P-Drawable sample-at draw-at]] ; P-Samplable+Drawable
   ;; [timelines.draw.defaults :refer :all]
   [timelines.macros :as macro]
   #_[timelines.draw.macros :refer [defrecord-graphic]])
  (:import
   [org.jetbrains.skija
    ;; BackendRenderTarget
    ;; Canvas
    ;; ColorSpace
    ;; DirectContext
    ;; FramebufferFormat
    Path
    Paint PaintMode
    Rect RRect
    Data
    Typeface Font
    Point
    ;; Surface
    ;; SurfaceColorFormat
    ;; SurfaceOrigin
    ]))
;; Macros
(do
  (defmacro defrecord-graphic [name params]
    (let [name (u/strip-symbol-ns-qualifiers name)]
      `(do
         (defrecord ~name ~params)
         (extend-type ~name
           ~'P-Samplable
           (~'sample-at [~'this ~'t]
             (timelines.utils/map-record #(if % (sample-at % ~'t)
                                              nil)
                                         ~'this)))))))

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

  (defrecord-graphic R-Paint [color alpha style stroke-width stroke-cap])

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

  (defn R-Paint->SKPaint
    "Make an actual SKPaint Java object from a Clojure R-Paint record"
    [{:keys [color style stroke-width alpha] :as paint}]
    (doto (Paint.)
      (.setColor (u/color (or color (paint-default-params :color))))
      (.setStrokeWidth (or stroke-width (paint-default-params :stroke-width)))
      (.setMode #_({:stroke (PaintMode/STROKE)
                    :fill  (PaintMode/FILL)})
       (or style (paint-default-params :style)))
      (.setAlphaf (or alpha (paint-default-params :alpha)))))

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

  (defrecord-graphic R-Rect [x y w h r paint])

  (defn rect
    ([x y w h]
     (rect x y w h nil nil))
    ([x y w h r]
     (rect x y w h r nil))
    ([x y w h r paint]
     (map->R-Rect {:x x :y y :w w :h h :r r :paint paint}))
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

  (defn load-font [path]
    (Font.
     (with-open [is (io/input-stream (io/resource path))]
       (let [bytes (.readAllBytes is)]
         (with-open [data (Data/makeFromBytes bytes)]
           (Typeface/makeFromData data 0))))))

  (def default-font (load-font "fonts/FiraCode-Regular.ttf"))

  (def default-text-paint (make-paint palette-white))

  (def default-text-size 20)

  (defn font-size [text s]
    (assoc text :size s))

  (defrecord-graphic R-Text [text x y size paint font])

  (defn make-text
    ([t x y]       (->R-Text t x y default-text-paint default-font))
    ([t x y s]     (->R-Text t x y s default-text-paint default-font))
    ([t x y s p]   (->R-Text t x y s p default-font))
    ([t x y s p f] (->R-Text t x y s p f)))

  (def text make-text)

  (extend-type R-Text
    P-Drawable
    (draw [{:keys [text x y paint font size]}]
      (let [paint (R-Paint->SKPaint paint)]
        (.drawString @*global-canvas text (float x) (float y) (.setSize font size) paint)))))

;; Lines
(do
  (defrecord-graphic R-Line [start end paint])

  (defn line
    ([start end] (line start end nil))
    ([start end paint]
     (->R-Line start end paint)))

  (extend-type R-Line
    P-Drawable
    (draw [this]
      (.drawLine @*global-canvas
                 (get-in this [:start 0])
                 (get-in this [:start 1])
                 (get-in this [:end 0])
                 (get-in this [:end 1])
                 (R-Paint->SKPaint
                  (or (:paint this) default-paint))))))

;; Polygons
(do
  (defrecord-graphic R-Polygon [points paint])

  (defn polygon
    ([points] (polygon points nil))
    ([points paint] (->R-Polygon points paint)))

  (defn R-Polygon->SKPoints [p]
    (into-array Point
                (map (fn [v] (Point. (float (first v)) (float (second v))))
                     (:points p))))

  (extend-type R-Polygon
    P-Drawable
    (draw [this]
      (.drawPolygon @*global-canvas
                    (R-Polygon->SKPoints this)
                    (R-Paint->SKPaint (or (:paint this) default-paint))))))

(do
  (extend-protocol P-Samplable
    org.jetbrains.skija.PaintMode
    (sample-at [this _]
      this)

    org.jetbrains.skija.Font
    (sample-at [this _]
      this)))
