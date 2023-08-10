(ns timelines.editor
  (:require  [clojure.test :as t]
             [clojure.walk :refer [prewalk postwalk]]
             [clojure.stacktrace :as stacktrace]
             [timelines.colors :refer :all]
             [timelines.graphics :refer :all]
             [timelines.api :refer :all]
             [timelines.editor.block :as b]
             [timelines.signal :refer :all]
             [timelines.protocols :refer [sample-at draw-now]]
             [timelines.globals :refer [screen-height screen-width]]
             [timelines.editor.state :refer :all]
             [timelines.time :refer [now]]
             [timelines.macros :refer [pprint]]))

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

(defn browser []
  (-> (rect browser-x browser-y browser-w browser-h)
      (apply-paint  (paint palette-blue-medium))))

(defn space  []
  (let [space-background (-> (rect space-x space-y space-w space-h)
                             (apply-paint (paint palette-blue-dark)))]
    [space-background (#'b/blocks)]))

(def note (from-list [0 0 3 0 5 5 0 8 (from-list [99 99 10])]))

(defn window []
  [(browser) (space)])

;; (def line-number 128)
  ;; (def file "~/test/path/to/file")

  ;; (def modeline [line-number file])
(defn modeline []
  (-> (rect modeline-x modeline-y screen-width modeline-height)
      (apply-paint (paint palette-blue-light))))

(defn scene []
  [(window)
   (modeline)])

(def fancy-broken-scene
  (let [color (from-list [black
                          blue
                          (from-list [black white black white] (fast 2 beat))
                          black
                          white]
                         (fast 2 bar))
        p (paint color)
        text-x (* bar (* 1.2 screen-width))
        text-y (/ screen-height 2)
        size (* 50 (-> t
                       (* twoPi)
                       sine01
                       (+ 0.5)))
        f (font size)]
    (text "BROKEN" text-x text-y f p)))

(text "broken" 10 20 (font 3))

(def broken-scene
  (-> (rect 0 0 screen-width screen-height)
      (apply-paint (paint palette-red))))

(defonce *broken? (atom false))
(defonce *stacktrace-printed? (atom false))

(defn draw-fps [n]
  (draw-now (text (str "FPS: " (int n)) 3 (- screen-height 3) (font 20))))

(defn draw-screen []
  (try
    (do
      (draw-now (scene))
      (reset! *broken? false)
      (reset! *stacktrace-printed? false))
    (catch Exception e
      (reset! *broken? true)
      (draw-now broken-scene)
      (when-not @*stacktrace-printed?
        (stacktrace/print-stack-trace (stacktrace/root-cause e))
        (reset! *stacktrace-printed? true)))))
