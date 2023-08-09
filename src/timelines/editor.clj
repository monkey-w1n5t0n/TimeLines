(ns timelines.editor
  (:require  [clojure.test :as t]
             [clojure.walk :refer [prewalk postwalk]]
             [clojure.stacktrace :as stacktrace]
             [timelines.colors :refer :all]
             [timelines.graphics :refer [apply-paint paint rect text]]
             [timelines.api :refer :all]
             [timelines.editor.block :as b]
             [timelines.signal :refer :all]
             [timelines.protocols :refer [sample-at draw-now]]
             [timelines.globals :refer [screen-height screen-width]]
             [timelines.editor.state :refer :all]
             [timelines.time :refer [now]]
             [timelines.macros :refer [pprint]]))

(do

;; example screen
  (def modeline-height 20)
  (def modeline-x 0)
  (def modeline-y (- screen-height modeline-height))

  (def browser-x 0)
  (def browser-y 0)
  (def browser-w 80)
  (def browser-h (- screen-height modeline-height))

  (def space-x (+ browser-x browser-w))
  (def space-y 0)
  (def space-w (- screen-width browser-w))
  (def space-h modeline-y)

  (def browser (-> (rect browser-x browser-y browser-w browser-h)
                   (apply-paint  (paint palette-blue-medium))))

  (def space
    (let [space-background (-> (rect  space-x space-y space-w space-h)
                               (apply-paint (paint palette-blue-dark)))]
      [space-background #'b/test-block]))

  (def note (from-list [0 0 3 0 5 5 0 8 (from-list [99 99 10])]))

  (def window [browser space])

;; (def line-number 128)
  ;; (def file "~/test/path/to/file")

  ;; (def modeline [line-number file])
  (def modeline (-> (rect modeline-x modeline-y screen-width modeline-height)
                    (apply-paint (paint palette-blue-light))))

  (def scene [window
              modeline]))

(def broken-scene
  [(-> (rect 0 0 screen-width screen-height)
       (apply-paint (paint palette-red))
       #_(apply-paint (paint red)))
   #_(let [color (from-list [black blue
                             (from-list [black white black white] (fast 2 beat))
                             black white]
                            (fast 2 bar))
           text-paint (paint color)
           text-x (* bar (* 1.2 screen-width))
           text-y (/ screen-height 2)
           text-size (* 50 (-> t
                               (* twoPi)
                               sine01
                               (+ 0.5)))]
       (text "BROKEN" text-x text-y text-size text-paint))])

(defonce *broken? (atom false))
(defonce *stacktrace-printed? (atom false))

(defn draw-fps [n]
  (draw-now (text (str "FPS: " (int n))
                  3
                  (- screen-height 3)
                  20
                  (paint black))))

(defn draw-screen []
  (try
    (do
      (draw-now scene)
      (reset! *broken? false)
      (reset! *stacktrace-printed? false))
    (catch Exception e
      (reset! *broken? true)
      (draw-now broken-scene)
      (when-not @*stacktrace-printed?
        (stacktrace/print-stack-trace (stacktrace/root-cause e))
        (reset! *stacktrace-printed? true)))))
