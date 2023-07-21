(ns timelines.api-test
  (:require  [clojure.test :as t]
             [clojure.walk :refer [prewalk postwalk]]
             [timelines.draw.graphics :refer :all]
             [timelines.signal.api :refer :all]
             [timelines.signal.core :refer :all]
             [timelines.protocols :refer [draw sample-at]]
             [timelines.globals :refer [screen-height screen-width]]
             [timelines.util.time :refer [now]]
             [timelines.util.macros :refer [pprint]]
             ))


(comment
  (defmacro defref [name expr]
    (let [vars (->> (tree-seq coll? seq expr)
                    (filter symbol?)
                    (remove #{'+ '- '* '/ '= 'not}))
          refs (map (fn [v] `(~v (var ~v))) vars)]
      `(do ~@refs (def ~name ~expr))))

  (def a 10)
  (def b 20)


  (pprint (defref c (+ a b)))



  (def c (+ #'a #'b))

  )


(defn draw-at [obj t]
  (-> obj (sample-at t) draw))

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
                   (paint (make-paint blue))))

  (def space (-> (rect  space-x space-y space-w space-h)
                 (paint (make-paint red))))


  (def name (from-list ["Bob" "Dave" "Nick" "Mary" "HeheheheHEHEHEHE"]
                       (mod1 (fast (from-list [1 1 2 3 1 4 3] (mod1 t))
                                   t))))

  (def window [browser
               space
               (text (str "Hello"
                          (from-list ["" " "] (fast (from-list [1 2 3]
                                                               (mod1 t))
                                                    (mod1 t)))
                          (from-list ["o" "iiii" "thoeu"]
                                     (mod1 t))) 100 100 10)
               ])

  ;; (def line-number 128)
  ;; (def file "~/test/path/to/file")

  ;; (def modeline [line-number file])
  (def modeline (-> (rect modeline-x modeline-y screen-width modeline-height)
                    (paint (make-paint black))))

  (def scene [window modeline])

  )

(defn draw-screen []
  (draw-at scene (now))
  ;; (draw-at (make-text "hello" 10 10) (now))
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
