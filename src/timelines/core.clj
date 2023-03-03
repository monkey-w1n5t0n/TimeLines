(ns timelines.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [timelines.editor.core :refer [draw-editor]]))

;; (defn scratch []
;;   (q/background 250))

;; ;; (defn sample-function [f, start, end, sr]
;; ;;   (let [
;; ;;         duration (- end start)
;; ;;         step (/ 1 sr)
;; ;;         domain (range start end step)]
;; ;;     (map vector domain (mapv f domain))))

;; ;; (sample-function (fn [t] (* 2 t)) 0 3 50)

;; ;; (defn scratch []
;; ;;   (q/background 120)
;; ;;   (let [f (fn [t] (sine t))
;; ;;         sr 500
;; ;;         dur 0.25
;; ;;         time-domain (range 0 (* dur sr))
;; ;;         vals (map f time-domain)
;; ;;         num-vals (count vals)
;; ;;         ]
;; ;;     (q/stroke 255)
;; ;;     (q/stroke-weight 2)

;; ;;     (q/begin-shape)

;; ;;     (doseq [[i v] (with-indices vals)]
;; ;;       (let [x (+ 0 (* 600 (/ i num-vals)))
;; ;;             y (+ 200 (* 100 v))]
;; ;;         (q/vertex x y))
;; ;;       )

;; ;;     (q/end-shape)
;; ;;     ))


;; (defn col-x [col]
;;   (+ buffer-left
;;      (* col col-width)
;;      (/ col-width 2)))

;; ;; (defn col-y [col]
;; ;;   (+ buffer-top
;; ;;      (* col col-height)
;; ;;      (/ col-height 2)))

;; ;; (defn row-x [row]
;; ;;   (+ buffer-left
;; ;;      (* row row-width)
;; ;;      (/ row-width 2)))

;; (defn row-y [row]
;;   (+ buffer-top
;;      (* row row-height)
;;      (/ row-height 2)))


;; (defn draw-point []
;;   (let [{:keys [px py]} (:point @*state)
;;         point-width   col-width
;;         point-height  row-height
;;         ]
;;     (color :point)
;;     (q/rect 10 10 point-width point-height 2)))

;; (defn draw-text []
;;   (q/background 255)
;;   (q/text-size 30)
;;   ;; (color :text)
;;   (q/text (:text @*state) 100 100)
;;   )

















;; (set-draw!
;;  (q/background 200 50 250)
;;  (draw-at (Rect. 100 100 200 200) 0.5)
;;  )




;;;;;
;;;;; SKETCH GLOBALS
;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn update-sketch-state [state]
  ;; (let [new-state
  ;;       (assoc state
  ;;              :t (q/seconds)
  ;;              :frame (q/frame-count))]
  ;;   (println (str "Frame: " (:frame state) ", queue: " @*queue))
  ;;   new-state)
  )

(defn setup-sketch []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (q/text-align :left :center)
  ;; (q/text-font
  ;;  (q/create-font "resources/fonts/FiraMono-Regular.ttf" 30 true))

  ;; (initialise-buffers 3 100 100)
  {:t (q/seconds)})

(defn draw-sketch [state]
  ;; error color
  (q/background 255 0 0)
  (draw-editor 500 500)
  ;; (let [margin 100
  ;;       w (- (q/width) margin)
  ;;       h (- (q/height) margin)]
  ;;   (q/stroke 0)
  ;;   (q/stroke-weight 2)
  ;;   (dotimes [i 50]
  ;;     (let [
  ;;           start (mapv (partial + margin)
  ;;                       (random-point w h))
  ;;           end (mapv (partial + margin)
  ;;                     (random-point w h))]
  ;;       (q/line start end))))
  ;; (q/fill 0)
  ;; (q/text-size 100)
  ;; (q/text "ERROR" 100 100)
  ;; (@*current-draw)
  ;; (draw-editor)
  ;; (scratch)
  ;; (text-test)
  )

(q/defsketch timelines
  :title "TimeLines"
  :size [500 500]
  :setup setup-sketch
  :update update-sketch-state
  :draw draw-sketch
  :features [:keep-on-top]
  :middleware [m/fun-mode])
