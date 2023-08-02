(ns timelines.graphics
  (:require
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [timelines.utils :as u]
   [timelines.colors :refer :all]
   [timelines.defaults :refer :all]
   [timelines.macros :refer [defs letmap pprint defgraphics]]
   [timelines.time :refer [now]]
   [timelines.globals :refer [*main-canvas]]
   [timelines.protocols :refer [P-Samplable P-Drawable P-Skijable ->skija draw sample-at draw-at]] ; P-Samplable+Drawable
   ;; [timelines.draw.defaults :refer :all]
   [timelines.macros :as macro]
   [timelines.specs :as specs]
   [clojure.spec.alpha :as s]
   #_[timelines.draw.macros :refer [defrecord-graphic]]
   [timelines.globals :as globals])
  (:import
   [org.jetbrains.skija Path PaintMode Data Typeface Font]))

(defgraphics Paint [color alpha style stroke-width stroke-cap]
  P-Skijable
  (->skija [{:keys [color style stroke-width alpha] :as paint}]
           (doto (org.jetbrains.skija.Paint.)
             (.setColor (u/color (or color default-color)))
             (.setStrokeWidth (or stroke-width default-stroke-width))
             (.setMode (or style default-paint-style))
             (.setAlphaf (or alpha 1.0)))))

(defgraphics Oval [x y w h]
  P-Skijable
  (->skija
   [this] (org.jetbrains.skija.Rect/makeXYWH x y w h))

  P-Drawable
  (draw [this] (draw this @timelines.globals/*main-canvas))
  (draw [{:keys [paint] :as oval} canvas]
        (let [oval (->skija oval)
              paint (->skija paint)]
          (.drawOval canvas oval paint))))

(defgraphics Rect [x y w h r]
  P-Skijable
  (->skija [this]
           (if (and r (> r 0))
             (org.jetbrains.skija.RRect/makeXYWH x y w h r)
             (org.jetbrains.skija.Rect/makeXYWH x y w h)))

  P-Drawable
  (draw [this] (draw this @timelines.globals/*main-canvas))
  (draw [{:keys [r paint] :as this} canvas]
        (let [rect (->skija this)
              paint (->skija  (or paint default-paint))
              draw-method (if (and r (> r 0))
                            org.jetbrains.skija.Canvas/.drawRRect
                            org.jetbrains.skija.Canvas/.drawRect)]
          (draw-method canvas rect paint))))

(defn font->skija [{:keys [name size path] :as f}]
  (let [typeface (with-open [is (io/input-stream (io/resource path))]
                   (let [bytes (.readAllBytes is)]
                     (with-open [data (Data/makeFromBytes bytes)]
                       (Typeface/makeFromData data 0))))]
    (Font. typeface size)))

;; Paints
(do
  ;; TODO

  (defn paint-style
    "Style keyword must be qualified, e.g. :paint-style/fill"
    [paint s]
    (assoc paint :style
           (case s
             :fill (PaintMode/FILL)
             :stroke (PaintMode/STROKE)
             :else default-paint-style)))

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
    ([] (map->Paint {}))
    ([color]
     (map->Paint {:color color}))
    ([color style]
     (->
      (map->Paint {:color color})
      (paint-style style)))
    ([color style stroke-width]
     (-> (map->Paint {:color color})
         (paint-style style)
         (paint-stroke-width stroke-width)))
    ([color style stroke-width & opts]
     (map->Paint
      {:color color :opts opts})))

  #_(def test-paint (-> (make-paint)
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

  (defn rect
    ([x y w h]
     (->Rect x y w h))
    ([x y w h r]
     (map->Rect {:x x :y y :w w :h h :r r}))
    ;; ([x y w h r1 r2]
    ;;  (map->R-Rect {:x x :y y :w w :h h :r1 r1 :r2 r2}))
    )

  (defn radius [obj r]
    (assoc obj :r r)
    #_([obj r1 r2]
       (assoc obj :r1 r1 :r2 r2)))

  (comment
    (def test-rect (rect 50 50 80 60))))

;; Text
(do

  (def default-font (load-font "fonts/FiraCode-Regular.ttf"))

  (def default-text-paint (make-paint palette-white))

  (def default-text-size 20)

  (defn font-size [text s]
    (assoc text :size s))

  (defgraphic Text [text x y size paint font]
    P-Skijable
    ;; TODO
    (->skija [this] this))

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
        (.drawString @*main-canvas text (float x) (float y) (.setSize font size) paint)))))

(do

  (extend-protocol P-Samplable
    org.jetbrains.skija.PaintMode
    (sample-at [this _]
      this)

    org.jetbrains.skija.Font
    (sample-at [this _]
      this)))
