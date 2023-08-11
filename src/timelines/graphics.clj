(ns timelines.graphics
  (:require [timelines.globals :refer [*main-canvas]]
            [timelines.protocols :refer :all]
            [timelines.colors :refer :all]
            [timelines.defaults :refer :all]
            [timelines.typography :as type]
            [timelines.utils :as u]
            [clojure.spec.alpha :as s])
  (:import [io.github.humbleui.skija PaintMode]
           #_[io.github.humbleui.types Rect RRect]))

;; Paint
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
    (->skija-impl [this]
      (doto (io.github.humbleui.skija.Paint.)
        (.setColor (->skija color))
        (.setStrokeWidth stroke-width)
        (.setMode (paint-style->skija style))
        (.setAlphaf alpha)))

    Object
    (toString [this]
      (str "Paint record: " (into {} this))))

  (def default-paint-map
    {:color default-color
     :alpha 1.0
     :style :fill
     :stroke-width 1.0
     :stroke-cap nil})

  (s/def ::paint #(instance? Paint %))

  ;; TODO @design this shouldn't really exist, should it?
  (defn paint
    ([] (map->Paint default-paint-map))
    ([color]
     (paint color nil nil))
    ([color & opts]
     (-> default-paint-map (into (apply hash-map opts)) (into {:color color}) map->Paint)))

  (def default-paint (paint))

  (defn apply-paint [obj paint]
    (assoc obj :paint paint))

  (comment
    (paint black :alpha 0.5 :style "hi")))

;; Rects
(do

  (defrecord Rect [x y w h]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

    P-Skijable
    (->skija-impl [this]
      (if-let [r (:r this)]
        (io.github.humbleui.types.RRect/makeXYWH x y w h r)
        (io.github.humbleui.types.Rect/makeXYWH x y w h)))

    P-Drawable
    #_(draw-impl [this] (draw this @timelines.globals/*main-canvas))
    (draw-impl [{:keys [r paint] :as this}]
      (let [rect (->skija this)
            paint (->skija paint)]
        (if (:r this)
          (.drawRRect @*main-canvas rect paint)
          (.drawRect @*main-canvas rect paint))))

    ;; P-Dimensions
    ;; (->height [this] h)
    ;; (->width [this] w)
    )

  (defn rect
    ([] (rect 0 0 20 20))
    ([w h]
     (rect 0 0 w h))
    ([x y w h & opts]
     (map->Rect (into (apply hash-map opts) {:x x :y y :w w :h h}))))

  (comment
    (rect 2 10))

  #_(defn rect-trbl [t r b l]
      (let [x
            y
            w
            h]
        (-> (rect x y w h)
            (assoc :t t :r r :b b :l l)))))

;; Circle & Oval
(do
  (defrecord Circle [x y r])

  (defn circle
    ([r] (map->Circle {:r r}))
    ([x y r]
     (map->Circle {:x x :y y :r r})))

  ;; Oval
  (defrecord Oval [x y w h]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

    P-Skijable
    (->skija-impl
      [this] (io.github.humbleui.types.Rect/makeXYWH x y w h))

    P-Drawable
    (draw-impl [{:keys [paint] :as oval}]
      (let [oval (->skija oval)
            paint (->skija paint)]
        (.drawOval @*main-canvas oval paint)))

    P-Dimensions
    (->height [this] h)
    (->width [this] w)))

(do
  (defrecord Line [start end])

  (defn line
    ([s e]
     (line s e {}))
    ([s e & opts]
     (map->Line (into (apply hash-map opts)
                      {:start s :end e})))))

;; Polygon
(do
  (defrecord Polygon [points])

  (defn polygon [points & opts]
    (map->Polygon (into (apply hash-map opts)
                        {:points points}))))

;; Font & Text
(do

  (defrecord Typeface [name]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

    P-Skijable
    (->skija-impl [this]
      (type/get-typeface name)))

  (defn typeface [name]
    (type/get-typeface name))

  (defrecord Font [name size]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

    P-Skijable
    (->skija-impl [this]
      (type/make-with-size (typeface name) size))

    Object
    (toString [this]
      (str "Font record: " (into {} this)))

    P-Dimensions
    ;; TODO @correctness verify this
    (->height [this] size))

  (defn font
    ([] (font default-font-name default-font-size))
    ([size] (font default-font-name size))
    ([name size] (->Font name size)))

  (defrecord Text [s x y])

  (defn text
    ([s] (text s 0 0 {}))
    ([s x y] (text s x y {}))
    ([s x y & opts]
     (map->Text (into (apply hash-map opts)
                      {:x x :y y :s s}))))

  (comment
    (-> (type/make-with-size (type/typeface-from-name "FiraCode Regular") 10) type/metrics)))
