(ns timelines.draw.graphics
  (:require
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [timelines.util.core :as u]
   [timelines.util.macros :refer [letmap pprint-macroexpand-1]]
   [timelines.util.time :refer [now]]
   [timelines.globals :refer [*global-canvas]]
   [timelines.protocols :refer [P-Samplable P-Drawable sample-at draw-at]] ; P-Samplable+Drawable
   [timelines.draw.utils :as draw-utils]
   ;; [timelines.draw.defaults :refer :all]
   [timelines.util.macros :as macro]
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
             (timelines.util.core/map-record #(if % (sample-at % ~'t)
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
                             :alpha} 1.0)

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
      (.setColor (draw-utils/color (or color (paint-default-params :color))))
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

(do

  (extend-protocol P-Samplable
    org.jetbrains.skija.PaintMode
    (sample-at [this _]
      this)

    org.jetbrains.skija.Font
    (sample-at [this _]
      this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE from here on unused
(comment

;; NOTE not used
  (defn color->SKColor [c]
    (draw-utils/color c))
  (def paint->SKPaint-map {:style {:paint-style/fill PaintMode/FILL
                                   :paint-style/stroke PaintMode/STROKE
                                   ;; TODO
                                   ;; :paint-style/stroke-and-fille PaintMODE/STROKE-AND-FILL
                                   }
                           ;; TODO
                           :stroke-cap {}})

  (defn make-point [])

  (defn paint [obj paint]
    (assoc obj :paint paint))

  ;; Colour
  (defn fill [obj col]
    (assoc obj :fill col))

  (defn stroke [obj col]
    (assoc obj :stroke col))

  ;; Geometric
  (defn scale [obj amt]
    (if (:scale obj)
      ;; TODO @dbg should be `(partial * amt)` or `(partial map * amt)`?
      (update obj :scale (partial map * amt))
      (assoc obj :scale amt)))

  (defn rotate [obj amt]
    (if (:rotate obj)
      (update obj :rotate (partial + amt))
      (assoc obj :rotate amt)))

  (defn translate [obj point]
    (if (:translate obj)
      (update obj :translate (partial map + point))
      (assoc obj :translate point)))

  (def effects '(fill stroke scale rotate translate))

;; NOTE should be called only on sampled shapes
  ;; currently enforced by the macro (?)
  (defn apply-effects [obj]
    (doseq [effect effects]
      (let [val ((keyword effect) obj)]
        (when val
          (apply (ns-resolve 'quil.core effect) (list val))))))

  (defn default-sampling-method [obj t]
    (update-map obj #(when % (sample-at % t))))

  (defrecord-graphic Shape-Entry [shape-name

                                  sampled-record
                                  sampled-autoconstr ; e.g. ->Sampled-Rect
                                  sampled-api-constr
                                  sampled-draw-method

                                  signal-record
                                  signal-autoconstr ; e.g. ->Signal-Rect
                                  signal-api-constr
                                  signal-sample-method
                                  signal-draw-method

                                  api-record
                                  api-constr])

  (defn shape-defined-symbols
    "Example usage:
  (shape-defined-symbols 'Rect)
  =>
  {:shape 'Rect
   :sampled-record 'Sampled-Rect
   :sampled-autoconstr '->Sampled-Rect
   :sampled-api-constr 'sampled-rect
   :sampled-draw-method 'draw-rect

   :signal-record 'Signal ; the default shape is always a signal
   :signal-autoconstr '->Signal-Rect
   :signal-api-constr 'signal-rect
   :signal-sample-method 'sample-rect-at
   :signal-draw-method 'draw-rect-at

   :api-record 'Rect
   :api-constr 'rect}"
    [shape-sym]
    (let [upper-str (name shape-sym)
          lower-str (str/lower-case upper-str)]
      (->
       (letmap shape-name upper-str

               ;; Sampled version
               sampled-record (str "Sampled-" shape-name)
               sampled-autoconstr (str "->" sampled-record)
               sampled-api-constr (str/lower-case sampled-record)
               ;; sampled-draw-method (str "draw-" sampled-api-constr)
               sampled-draw-method (str "draw-" lower-str)

               ;; Signal version
               signal-record upper-str
               signal-autoconstr (str "->" signal-record)
               signal-api-constr (str/lower-case signal-record)
               signal-sample-method (str "sample-" lower-str "-at")
               signal-draw-method (str "draw-" lower-str "-at")

               api-record signal-record
               api-constr signal-api-constr)
       (update-map symbol)
       map->Shape-Entry)))

  (comment
    (shape-defined-symbols 'Rect))

  (defmacro defrecord-shape [shape keys sampled-draw-fn & opt-sampling-method]
    "Example usage: (defrecord-shape Rect [x y w h] (q/rect x y w h))"
    (let [sampling-method (if (and opt-sampling-method (= 1 (count opt-sampling-method)))
                            (first opt-sampling-method)
                            'default-sampling-method)

          ;; TODECIDE @completeness decide if these should be different
          sampled-keys keys
          signal-keys keys

          {:keys [shape-name

                  sampled-record
                  sampled-autoconstr
                  sampled-api-constr
                  sampled-draw-method

                  signal-record
                  signal-autoconstr
                  signal-api-constr
                  signal-sample-method
                  signal-draw-method

                  api-record
                  api-constr]} (shape-defined-symbols shape)]
      `(do
         ;; Declare records for sampled and signal versions
         (defrecord ~sampled-record ~sampled-keys)
         (defrecord ~signal-record  ~signal-keys)

         ;; Convenience constructors for creating versions directly (TODECIDE probably not useful)
         (def ~sampled-api-constr ~sampled-autoconstr)
         (def ~signal-api-constr ~signal-autoconstr)

         ;; Sampling method to go from signal -> sampled
         ;; TODO use specter or something
         (defn ~signal-sample-method [shape# t#]
           (-> shape#
               (update-map #(when % (sample-at % t#)))
               ~(symbol (str "map->" (name sampled-record)))))

         ;; Drawing methods for each (e.g.`draw-rect` and `draw-rect-at` respectively)
         (defn ~sampled-draw-method [{:keys ~sampled-keys :as this#}]
           ;; (apply)
           ~sampled-draw-fn)

         ;; (def ~sampled-draw-method ~draw-sampled-fn)
         (defn ~signal-draw-method  [shape# t#]
           (-> shape# (sample-at t#) timelines.protocols/draw))

         ;; Implement
         (extend-type ~sampled-record
           P-Drawable
           (draw [this#] (~sampled-draw-method this#)))

         (extend-type ~signal-record
           P-Samplable
           (sample-at [this# t#] (~signal-sample-method this# t#))

           P-Samplable+Drawable
           (draw-at [this# t#] (-> this# (sample-at t#) draw))))))

  (comment

    (-> '(defrecord-shape Rect [x y w h] (.drawRect * x y w h))
        macroexpand-1
        ;; util/strip-symbol-ns-qualifiers
        pprint))

;; RECT
  (defrecord Rect [x y w h]
    P-Drawable
    (draw [this]
      ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SHAPE-DEFINING MACROS
  ;; needs some work still
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
  ;; Arc
;;;;;;;;;

  (defrecord-shape Arc [x y w h start stop]
    (q/arc x y w h start stop))

;;;;;;;;;
  ;; Ellipse
;;;;;;;;;

  (defrecord-shape Ellipse [x y w h]
    (q/ellipse x y w h))

;;;;;;;;;
  ;; Circle
;;;;;;;;;

  (defrecord-shape Circle [x y r]
    (q/ellipse x y r r))

;;;;;;;;;
  ;; Line
;;;;;;;;;

  (defrecord-shape Line [p1 p2]
    (q/line p1 p2))

;;;;;;;;;
  ;; Quad
;;;;;;;;;

  (defrecord-shape Quad [p1 p2 p3 p4]
    (apply q/quad (apply concat [p1 p2 p3 p4])))

;;;;;;;;;
  ;; RECT
;;;;;;;;;
  ;; TODO @completeness add (optional) radius parameters

  (defrecord-shape Rect [x y w h]
    (q/rect x y w h))

;;;;;;;;;
  ;; Triangle
;;;;;;;;;

  (defrecord-shape Triangle [p1 p2 p3]
    (apply q/triangle (apply concat [p1 p2 p3])))

;;;;;;;;;
  ;; Text
;;;;;;;;;

  (defrecord-shape Text [s x y]
    (q/text s x y)))
