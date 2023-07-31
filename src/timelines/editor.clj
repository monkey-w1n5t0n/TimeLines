(ns timelines.editor
  (:require  [clojure.test :as t]
             [clojure.walk :refer [prewalk postwalk]]
             [clojure.stacktrace :as stacktrace]
             [timelines.graphics :refer :all]
             [timelines.api :refer :all]
             [timelines.signal :refer :all]
             [timelines.protocols :as p]
             [timelines.globals :refer [screen-height screen-width]]
             [timelines.time :refer [now]]
             [timelines.macros :refer [pprint]]))

(comment (.getTextWidth))

(comment
  (defmacro defref [name expr]
    (let [vars (->> (tree-seq coll? seq expr)
                    (filter symbol?)
                    (remove #{'+ '- '* '/ '= 'not}))
          refs (map (fn [v] `(~v (var ~v))) vars)]
      `(do ~@refs (def ~name ~expr)))))

(do
  (def BPM 120)
  (def meter 4)

  ;; Beat duration
  (def beatDur (/ 60 BPM))

  ;; Bar duration
  (def barDur (* beatDur meter))

  ;; Timed phasors
  (def beat
    (mod1 (/ t beatDur)))

  (def bar
    (mod1 (/ t barDur)))

  ;; Timed phasor that goes between 0 and meter
  (def beat-n
    (mod (/ t beatDur) meter))

  ;; Stepped counters
  (def beatNum
    (int (/ t beatDur)))

  (def barNum
    (int (/ t barDur))))

(defn draw-at [obj t]
  (-> obj (p/sample-at t) p/draw))

;; example screen
(do
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
                   (paint (make-paint palette-blue-medium))))

  (def amp-expr '(+ 0.5 (* 0.2 (sine01 (* twoPi (slow 2 bar))))))
  (def melody-expr '(from-list [0 0 3 0 2 2 5 12] bar))

  (def amp-sig (eval amp-expr))
  (def melody-sig (eval melody-expr))
  (def freq-expr (list 'midinote->freq melody-expr))

  (def test-node
    (let [node-x 100
          node-y 20
          node-width 590
          node-height 200
          node-background-paint (make-paint palette-blue-medium)
          node-stroke-paint (make-paint palette-red palette-red :stroke 2)
          param-size 25
          expr-size 18
          key-paint (make-paint palette-red)
          text-paint (make-paint palette-white)
          background (-> (rect node-x node-y node-width node-height 20)
                         (paint node-background-paint))
          parameters
          [;; amp
           (let [x (+ node-x 10)
                 y (+ node-y 30)]
             [(text ":amp" x y param-size key-paint)
              #_(text #_(clojure.core/str (sample-at amp-expr (now))))
              (text (str amp-sig) (+ x 80) y expr-size text-paint)
              (text (clojure.core/str amp-expr) (+ x 10) (+ y param-size) expr-size text-paint)])

           ;; freq
           (let [x (+ node-x 10)
                 y (+ node-y 100)]
             [(text ":freq" x y param-size key-paint)
              (text (str melody-sig) (+ x 80) y expr-size text-paint)
              (text (clojure.core/str freq-expr) (+ x 10) (+ y param-size) expr-size text-paint)])]]

      [background parameters]))

  (def space
    (let [space-background (-> (rect  space-x space-y space-w space-h)
                               (paint (make-paint palette-blue-dark)))]
      [space-background test-node]))

  (def name (from-list ["Bob" "Dave" "Nick" "Mary" "HeheheheHEHEHEHE"]
                       (mod1 (fast (from-list [1 1 2 3 1 4 3] (mod1 t))
                                   t))))

  (def note (from-list [0 0 3 0 5 5 0 8 (from-list [99 99 10])]))

  (def window [browser
               space
               #_(text (str "Hello"
                            #_(from-list [" " "" " "] bar)
                            ".") (* 100 bar) 100 50)])

;; (def line-number 128)
  ;; (def file "~/test/path/to/file")

  ;; (def modeline [line-number file])
  (def modeline (-> (rect modeline-x modeline-y screen-width modeline-height)
                    (paint (make-paint palette-blue-light))))

  (def scene [window
              modeline]))

(def broken-scene
  [(-> (rect 0 0 screen-width screen-height)
       (paint (make-paint red)))
   (let [color (from-list [black blue
                           (from-list [black white black white] (fast 2 beat))
                           black white]
                          (fast 2 bar))
         text-paint (make-paint color)
         text-x (* bar (* 1.2 screen-width))
         text-y (/ screen-height 2)
         text-size (* 50 (-> t
                             (* twoPi)
                             sine01
                             (+ 0.5)))]
     (text "BROKEN" text-x text-y text-size text-paint))])

(defonce *broken? (atom false))
(defonce *stacktrace-printed? (atom false))

(defn draw []
  (try
    (do
      (draw-at scene (now))
      (reset! *broken? false)
      (reset! *stacktrace-printed? false))
    (catch Exception e
      (reset! *broken? true)
      (draw-at broken-scene (now))
      (when-not @*stacktrace-printed?
        (stacktrace/print-stack-trace (stacktrace/root-cause e))
        (reset! *stacktrace-printed? true)))))

;; (defn draw-windows []
;;   (let []
;;     (doseq [win (windows)]
;;       (let [
;;             x (+ (window-x win) (half window-padding))
;;             y (+ ( window-y  win ) (half window-padding))
;;             w (- ( window-w  win ) (half window-padding))
;;             h (- (window-h win) (half window-padding))
;;             ]
;;         (color :background)
;;         (q/rect x y w h)

;;         ))))

;; (defn draw-background []
;;   ;; (color :background)
;;   (q/fill 0)
;;   (q/rect-mode :corner)
;;   (q/rect 0 0 window-width window-height))

;; (defn draw-infoline []
;;   (let [x 0
;;         y (- window-height infoline-height)]
;;     (q/fill 100)
;;     (q/no-stroke)
;;     (q/rect x y infoline-width infoline-height)

;;     ))
