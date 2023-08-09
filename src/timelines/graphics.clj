(ns timelines.graphics
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [timelines.utils :as u]
   [timelines.colors :refer :all]
   [timelines.defaults :refer :all]
   [timelines.macros :refer [defs letmap pprint defgraphics]]
   [timelines.time :refer [now]]
   [timelines.globals :refer [*main-canvas]]
   [timelines.protocols :refer [P-Samplable P-Drawable P-Skijable ->skija ->skija-impl draw draw-impl sample-at-impl sample-at draw-at]] ; P-Samplable+Drawable
   [timelines.macros :as macro]
   [timelines.debug :refer [*dbg]]
   [timelines.skija :as sk]
   [timelines.specs :as specs]
   [clojure.spec.alpha :as s]
   [timelines.globals :as globals]
   [timelines.skija :as sk])
  (:import
   [org.jetbrains.skija Path Canvas PaintMode Typeface Font]))

(sk/init)

;; Container
(do
  (defgraphics Container [x y contents]
    P-Drawable
    (draw-impl [this]
               (doseq [x contents]
                 (when x
                   (draw x))))))

;; Shapes
(do
  ;; Rect

  (defgraphics Rect [x y w h r]
    P-Skijable
    (->skija-impl [this]
                  (if (and r (> r 0))
                    (org.jetbrains.skija.RRect/makeXYWH x y w h r)
                    (org.jetbrains.skija.Rect/makeXYWH x y w h)))

    P-Drawable
    #_(draw-impl [this] (draw this @timelines.globals/*main-canvas))
    (draw-impl [{:keys [r paint] :as this}]
               (let [rect (->skija this)
                     paint (->skija  (or paint default-paint))]
                 (if (and r (> r 0))
                   (.drawRRect @*main-canvas rect paint)
                   (.drawRect @*main-canvas rect paint)))))
  (defn rect
    ([x y w h]
     (map->Rect {:x x :y y :w w :h h}))
    ([x y w h r]
     (map->Rect {:x x :y y :w w :h h :r r})))

  (defn radius [obj r]
    (assoc obj :r r))

  (comment
    (def test-rect (rect 50 50 80 60)))

  ;; Oval
  (defgraphics Oval [x y w h]
    P-Skijable
    (->skija-impl
     [this] (org.jetbrains.skija.Rect/makeXYWH x y w h))

    P-Drawable
    (draw-impl [{:keys [paint] :as oval}]
               (let [oval (->skija oval)
                     paint (->skija paint)]
                 (.drawOval @*main-canvas oval paint)))))

;; Paints
(do
  (s/def :paint/style #{:fill :stroke})
  (s/def :paint/stroke-width number?)

  (defn paint-style->skija [style]
    (case style
      :fill (PaintMode/FILL)
      :stroke (PaintMode/STROKE)
      :else (throw (Exception. (str "Incorrect paint style: " style)))))

  (defgraphics Paint [color alpha style stroke-width stroke-cap]
    P-Skijable
    (->skija-impl [{:keys [color style stroke-width alpha] :as paint}]
                  (doto (org.jetbrains.skija.Paint.)
                    (.setColor (u/color (or color default-color)))
                    (.setStrokeWidth (or stroke-width default-stroke-width))
                    (.setMode (paint-style->skija (or style default-paint-style)))
                    (.setAlphaf (or alpha 1.0))))
    Object
    (toString [this]
              (str "Paint record: " (into {} this))))

  (s/def ::paint #(instance? Paint %))

  ;; TODO @design this shouldn't really exist, should it?
  (extend-protocol P-Skijable
    org.jetbrains.skija.Paint
    (->skija-impl [this] this))

  (defn paint
    ([color]
     (paint color nil nil))
    ([color style]
     (paint color style nil))
    ([color style stroke-width]
     (map->Paint {:color color :style style :stroke-width stroke-width})))

  (defn apply-paint [obj paint]
    (assoc obj :paint paint))

  #_(defn paint
      ;; Default paint
      ([] (map->Paint {}))
      ;; Paint with color
      ([color]
       (map->Paint {:color color}))
      ;; Paint and either style or obj
      ([color-or-paint style-or-obj]
       (if (s/valid? ::paint color-or-paint)
         (assoc style-or-obj :paint color-or-paint)
         (if (s/valid? :paint/style style-or-obj)
           (map->Paint {:color color-or-paint :style style-or-obj})
           (paint color-or-paint nil nil style-or-obj))))
      ([color style stroke-width-or-obj]
       (if (s/valid? :paint/stroke-width stroke-width-or-obj)
         (map->Paint {:color color :style style :stroke-width stroke-width-or-obj})
         (paint color style nil stroke-width-or-obj)))
      ([color style stroke-width obj]
       (assoc obj :paint
              (map->Paint {:color color :style style :stroke-width stroke-width})))))

;; Text
(do

  (def default-text-paint (paint palette-white))
  (def default-text-size 20)

  (defn make-with-size ^Font [^Typeface typeface size]
    (Font. typeface (float size)))

  (defn make-skija-font [name size]
    (when @*dbg
      (println (str "make-skija-font args: " name ", " size)))
    (make-with-size (@sk/*font-cache name) size))

  (defgraphics R-Font [name size]
    P-Skijable
    (->skija-impl [this]
                  (make-skija-font name size))
    Object
    (toString [this]
              (str "Font record: " (into {} this))))

  (def default-font-name   "FiraCode Regular")

  (defn font [name size]
    (->R-Font name size))

  (defgraphics Text [text x y size paint font]
    ;; P-Skijable
    ;; (->skija [this] (Text.))
    P-Drawable
    (draw-impl [this]
               (.drawString @*main-canvas
                            text
                            (float x)
                            (float y)
                            (->skija font)
                            (->skija paint)))

    Object
    (toString [this]
              (str "Text record: " (into {} this))))

  (defn text
    ([t x y]
     (text t x y nil nil nil))
    ([t x y s]
     (text t x y s nil nil))
    ([t x y s p]
     (text t x y s p nil))
    ([t x y s p f]
     (let [s (or s default-text-size)
           p (or p default-text-paint)
           f (or f (font default-font-name s))]
       (->Text t x y s p f)))))

;; Misc protocols
(do

  (extend-protocol P-Samplable
    org.jetbrains.skija.PaintMode
    (sample-at-impl [this _]
      this)

    org.jetbrains.skija.Font
    (sample-at-impl [this _]
      this)))
