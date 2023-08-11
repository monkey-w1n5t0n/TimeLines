(ns timelines.colors
  (:require [timelines.protocols :refer :all]
            [timelines.utils :as u]
            [timelines.api :as api]))

(defn hex->color [^long l]
  (.intValue (Long/valueOf l)))

(defn rgba->long [^double r ^double g ^double b ^double a]
  (let [a (-> 0xFF000000 (* a) long)
        r (-> 0x00FF0000 (* r) long)
        g (-> 0x0000FF00 (* g) long)
        b (-> 0x000000FF (* b) long)]
    (long (+ r g b a))))

(defn rgba->color
  ([r g b] (rgba->color r g b 1.0))
  ([r g b a] (hex->color (rgba->long r g b a))))

(def white  (hex->color 0xFFFFFFFF))
(def black (hex->color 0xFF000000))
(def blue  (hex->color 0xFF0000ff))
(def green  (hex->color 0xFF00ff00))
(def grey  (hex->color 0xFF333333))
(def red   (hex->color 0xFFFF0000))

;; Nord-ish
(def palette-red         (hex->color 0xFFE63946))
(def palette-white       (hex->color 0xFFA8DADC))
(def palette-blue-light (hex->color 0xFFA8DADC))
(def palette-blue-medium (hex->color 0xFF457B9D))
(def palette-blue-dark (hex->color 0xFF1D3557))

(do
  (defrecord Color [r g b a]
    P-Samplable
    (sample-at-impl [this t]
      (u/map-record #(sample-at % t) this))

    P-Skijable
    (->skija-impl
      [this] (rgba->color r g b a)))

  (defn color
    ([r g b] (color r g b 1.0))
    ([r g b a]
     (->Color r g b a))))

(comment

  (-> (color 0.2 1 (api/mod1 api/t) 4) (sample-at 10.05)))
