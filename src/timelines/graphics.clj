(ns timelines.graphics
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [timelines.utils :as u]
   [timelines.colors :refer :all]
   [timelines.defaults :refer :all]
   [timelines.macros :refer [defs letmap pprint]]
   [timelines.time :refer [now]]
   [timelines.api :as api]
   [timelines.globals :refer [*main-canvas]]
   [timelines.protocols :refer :all] ;; [P-Dimensions P-Samplable P-Drawable P-Skijable ->skija ->skija-impl draw draw-impl sample-at-impl sample-at draw-at]
                                        ; P-Samplable+Drawable
   [timelines.macros :as macro]
   [timelines.debug :refer [*dbg]]
   [timelines.skija :as sk]
   [timelines.specs :as specs]
   [timelines.globals :as globals]
   [clojure.pprint :as pprint])

  (:import
   [org.jetbrains.skija Path Canvas PaintMode Typeface Font]))

(declare default-paint)

(sk/init)

(defmacro with-translation [x y & block]
  `(do (.save @*main-canvas)
       (.translate @*main-canvas ~x ~y)
       ~@block
       (.restore @*main-canvas)))

;; Container
;; TODO
(do
  (def container-padding-x 0)
  (def container-padding-y 0)

  (defrecord Container [x y children]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

    P-Drawable
    (draw-impl [this]
      (with-translation x y
        (doseq [c children]
          (when c (draw c)))))

    P-Dimensions
    (get-height-impl [this]
      (apply api/max (mapv get-height children)))

    (get-width-impl [this]
      (let [min-x (apply api/min (mapv :x children))
            max-x (apply api/max (mapv #(api/+ (:x %)
                                               (get-width %))
                                       children))]
        (api/- max-x min-x))))

  (defn container [x y children]
    (->Container x y (into [] children))))

;; Vectors
(do
  (extend-type clojure.lang.PersistentVector
    P-Dimensions
    (get-height-impl [this]
      (let [min-y (->> this (map :y) (apply api/min))
            max-y (apply api/max
                         (map #(api/+ (:y %) (get-height %))
                              this))]
        (api/abs (api/- max-y min-y))))
    (get-width-impl [this]
      (let [min-x (apply api/min (map :x this))
            max-x (apply api/max (map #(api/+ (:x %)
                                              (get-width %))
                                      this))]
        (api/abs (api/- max-x min-x))))))

(comment

;;
  )

;; Shapes
(do
  ;; Rect
  (defrecord Rect [x y w h r]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

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
          (.drawRect @*main-canvas rect paint))))

    P-Dimensions
    (get-height-impl [this] h)
    (get-width-impl  [this] w))

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
  (defrecord Oval [x y w h]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

    P-Skijable
    (->skija-impl
      [this] (org.jetbrains.skija.Rect/makeXYWH x y w h))

    P-Drawable
    (draw-impl [{:keys [paint] :as oval}]
      (let [oval (->skija oval)
            paint (->skija paint)]
        (.drawOval @*main-canvas oval paint)))

    P-Dimensions
    (get-height-impl [this] h)
    (get-width-impl [this] w)))

;; Paints
(do
  (s/def :paint/style #{:fill :stroke})
  (s/def :paint/stroke-width number?)

  (defn paint-style->skija [style]
    (case style
      :fill (PaintMode/FILL)
      :stroke (PaintMode/STROKE)
      :else (throw (Exception. (str "Incorrect paint style: " style)))))

  (defrecord Paint [color alpha style stroke-width stroke-cap]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

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

  (def default-paint
    (paint black)))

;; Text
(do
  (def default-text-paint (paint palette-white))
  (def default-text-size 20)

  (defn make-with-size ^Font [^Typeface typeface size]
    (Font. typeface (float size)))

  (defn make-skija-font [name size]
    (make-with-size (@sk/*font-cache name) size))

  (defrecord R-Font [name size]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

    P-Skijable
    (->skija-impl [this]
      (make-skija-font name size))

    Object
    (toString [this]
      (str "Font record: " (into {} this)))

    P-Dimensions
    ;; TODO @correctness verify this
    (get-height-impl [this] size))

  (def default-font-name "FiraCode Regular")
  (def default-font-size 20)

  (defn font
    ([size] (font default-font-name size))
    ([name size]
     (->R-Font name size)))

  (def default-text-font (font default-font-name default-font-size))
  (def default-text-paint (paint black))

  (defrecord Text [text x y font paint]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

    P-Drawable
    (draw-impl [this]
      (.drawString @*main-canvas
                   text
                   (float x)
                   (float y)
                   (->skija font)
                   (->skija paint)))

    P-Dimensions
    (get-height-impl [this] (get-height font))
    (get-width-impl [this]
      (let [bounding-box  (.measureText (->skija font) text)
            left (._left bounding-box)
            right (._right bounding-box)]
        (- right left)))

    Object
    (toString [this]
      (str "Text record: " (into {} this))))

  (defn text-size [t]
    (-> t :font :size))

  (defn text
    ([t x y]
     (text t x y nil nil))
    ([t x y f]
     (text t x y f nil))
    ([t x y font paint]
     (let [font (or font default-text-font)
           paint (or paint default-text-paint)]
       (->Text t x y font paint)))))

;; Misc protocols
(do

  (extend-protocol P-Samplable
    org.jetbrains.skija.PaintMode
    (sample-at-impl [this _]
      this)

    org.jetbrains.skija.Font
    (sample-at-impl [this _]
      this)))

(comment

  (def a #timelines.graphics.Paint{:color 4294967295, :alpha nil, :style nil, :stroke-width nil, :stroke-cap nil})
  (sample-at a 2)

;;
  )
