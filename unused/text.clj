(ns timelines.text
(:require [quil.core :as q]
            ;; [quil.middleware :as m]
            ;; [clojure.walk :as w]
            ;; Internal
            [timelines.parameters :refer :all]
            [timelines.utils :refer :all])
  )


(defn text-test []
  (let [s "Sphinx!"
        x 40 y 150
        size 250
        ]
    (q/background 21)

    (q/stroke 255 0 0)

    ;; Ascender height
    (let [x 0
          y (+ y (* size ascender-relative-offset))]
      (q/line [x y] [(q/width) y]))
    ;; Capital height
    (let [x 0
          y (+ y (* size capital-relative-offset))]
      (q/line [x y] [(q/width) y]))
    ;; Median
    (let [x 0
          y (+ y (* size median-relative-offset))]
      (q/line [x y] [(q/width) y]))
    ;; Baseline
    (let [x 0
          y (+ y (* size baseline-relative-offset))]
      (q/line [x y] [(q/width) y]))
    ;; Descender
    (let [x 0
          y (+ y (* size descender-relative-offset))]
      (q/line [x y] [(q/width) y]))




;;     (q/fill 200)
;;     (q/text-size size)
;;     (q/text s x y)

;;     ))
