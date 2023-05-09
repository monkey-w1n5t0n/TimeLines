(ns timelines.core
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [timelines.protocols :refer :all]
   [timelines.signal.core :refer :all]
   [timelines.signal.api :refer :all]
   [timelines.draw.graphics :refer :all]
   [timelines.editor.core :refer [draw-editor]]))

(def *state (atom {:time 0}))

(defn now []
  (:time @*state))

;; (defn draw-at-now [x]
;;   (draw-at x (now)))

(defn update-sketch-state [state]
  ;; (swap! *state conj :time (clojure.core// (q/millis)))
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
  (q/background 55 0 0)
  (q/fill 255)
  (draw-at (rect 100 100 (+ 30 (* 20 (sin t))) 50)
           (clojure.core// (q/millis) 1000))


  (draw-at (text (str "he"
                      (from-list ["h" "oo" "aoeuth,pr9(99"] (slow 3 (mod1 t)))
                      (from-list ["  2s" ", -HEEEHEHE" "..."] (slow 2 (mod1 t)))
                      )
                 10
                 20)
           (clojure.core// (q/millis) 1000))
  )


(q/defsketch timelines
  :title "TimeLines"
  :size [1000 1000]
  :setup setup-sketch
  :update update-sketch-state
  :draw draw-sketch
  :features [:keep-on-top]
  :middleware [m/fun-mode])
