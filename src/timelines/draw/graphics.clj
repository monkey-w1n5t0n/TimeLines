(ns timelines.draw.graphics
  (:require [quil.core :as q]
            ;; [clojure.core :as c]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [timelines.util.macros :refer [letmap pprint-macroexpand-1]]
            [timelines.signal.core :refer :all]
            [timelines.signal.api :refer [* +]]
            ;; [timelines.signal.api :refer :all]
            [timelines.protocols :refer :all] ; [P-Samplable P-Drawable P-Samplable+Drawable sample-at]
            [timelines.util.core :refer :all]
            [timelines.util.core :as util]))

(defn draw-at-now [x]
  (draw-at x (clojure.core// (q/millis))))

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
    (let [val ((keyword effect)obj)]
      (when val
        (apply (ns-resolve 'quil.core effect) (list val))))))

(defn default-sampling-method [obj t]
  (update-map obj #(when % (sample-at % t))))

(defrecord Shape-Entry [shape-name

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
  (shape-defined-symbols 'Rect)
  )

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

  (-> '(defrecord-shape Rect [x y w h] (q/rect x y w h))
      macroexpand-1
      ;; util/strip-symbol-ns-qualifiers
      pprint)

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SHAPES
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
    (q/text s x y))
