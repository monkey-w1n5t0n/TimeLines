(ns timelines.api-test
  (:require  [clojure.test :as t]
             [clojure.walk :refer [prewalk postwalk]]
             [timelines.draw.graphics :refer :all ;; [make-paint paint rect color]
              ]
             [timelines.signal.api :refer :all]
             [timelines.signal.core :refer :all]
             [timelines.protocols :refer [draw sample-at]]
             [timelines.globals :refer [screen-height screen-width]]
             [timelines.util.time :refer [now]]
             ))



(def modeline-height (+ 80 (* 20 (sin (* 2 Math/PI t)))))

(def browser-x 0)
(def browser-y modeline-height)
(def browser-w 20)
(def browser-h (- screen-height modeline-height))

(def space-x (+ browser-x browser-w))
(def space-y modeline-height)
(def space-w (- screen-width browser-w))
(def space-h (- screen-height modeline-height))

(def browser (-> (rect browser-x browser-y browser-w browser-h)
                 (paint (make-paint blue 1))))

(def space (-> (rect space-x space-y space-w space-h)
               (paint (make-paint red 3))))

(def window [browser space])

;; (def line-number 128)
;; (def file "~/test/path/to/file")

;; (def modeline [line-number file])
(def modeline (-> (rect 0 0 screen-width modeline-height)
                  (paint (make-paint black 4))))

(def scene [window modeline])

(defn draw-at [obj t]
  (-> obj (sample-at t) draw))

(defn draw-screen []
  ;; (println (now))
  (draw-at scene (now))
  ;; (draw-at (rect (+ 30 (* 10 (fast 3 (sin t)))) 10 20 20) (/ (now) 1000))
  )

(comment

  (def data (rect t (+ t 1) (+ t 2) (+ t 3)))

  (sample-at data 3)

  (postwalk #(println
              (str "Now visiting: " %
                   ",/nSampled value: "
                   (sample-at 100 %)))
            data)


  (map (partial * 2) (rect 1 2 3 4)))
