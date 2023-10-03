(ns timelines.editor.block
  (:require [timelines.graphics :as g]
            [timelines.graphics :refer [text-size container apply-paint paint rect text font]]
            [timelines.globals :refer :all]
            [timelines.colors :refer :all]
            [timelines.editor.state :refer :all]
            [timelines.api :refer :all]))

(do

  (def param-font (font "FiraCode Regular" 25))

  ;; TODO
  (defn block [expr]
    nil)

  (defn blocks []
    (->
     (block '(d1 (sqr (fast (fromList [1 2 3 4] bar)
                            bar))))
     (assoc :x 200 :y 100))))

;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
(comment

  ;; TODO
  (defn param-block [param code style]
    (let [param (text 0 0 (name param))
          code code
          vis (rect 10 10 10 10)]
      (g/container x y param vis code)))

  (defn block [name param-map]
    (let [n-params (count (keys param-map))
          y-offset (+ block-line-height 10)
          param-blocks (for [[param {:keys [code style]}] param-map]
                         (param-block param code style))]
      (apply g/container param-blocks)))

  (def test-block
    (let [node-x 100
          node-y 20
          node-width 590
          node-height 200
          node-background-paint (paint palette-blue-medium)
          node-stroke-paint (paint palette-red :stroke 2)
          param-size 25
          expr-size 18
          key-paint (paint palette-red)
          text-paint (paint palette-white)
          background (-> (rect node-x node-y node-width node-height 20)
                         (apply-paint node-background-paint))
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

      [background parameters])))
