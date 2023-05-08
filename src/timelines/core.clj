(ns timelines.core
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [timelines.protocols :refer :all]
   [timelines.signal.core :refer :all]
   [timelines.signal.api :refer :all]
   [timelines.draw.graphics :refer :all]
   [timelines.editor.core :refer [draw-editor]]))

(defn now []
  (clojure.core// (q/millis) 1000))

(defn update-sketch-state [state]
  state)

(defn setup-sketch []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  (q/text-align :left :center)
  (q/text-size 90)
  ;; (q/text-font
  ;;  (q/create-font "resources/fonts/FiraMono-Regular.ttf" 30 true))
  {})

(defn draw-sketch [state]
  ;; error color
  (q/background 255 0 0)
  (q/stroke 0)
  (q/fill 0)
  (let [s (str "hello " (sample-at
                         (from-list ["bob" "dave"]
                                    (mod1 t))
                         (now)))]
    (q/text s 50 80))
  )

(q/defsketch timelines
  :title "TimeLines"
  :size [500 500]
  :setup setup-sketch
  :update update-sketch-state
  :draw draw-sketch
  :features [:keep-on-top]
  :middleware [m/fun-mode])
