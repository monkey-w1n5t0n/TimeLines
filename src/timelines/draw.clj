(ns timelines.draw
  (:require [timelines.utils :as u]
            [timelines.api :refer :all])
  #_(:import
     [io.github.humbleui.skija Paint]
     [io.github.humbleui.types Rect RRect]))

;; (defn draw []
;;   (.drawRect  (Rect/makeXYWH 200 300 400 400) (doto (Paint.) (.setColor (u/color 0xFFF04FFF)))))

#_(defn make-xywh
    ([x y w h] (make-xywh x y w h nil))
    ([x y w h r]
     (if r
       (RRect/makeXYWH x y w h r)
       (Rect/makeXYWH x y w h))))

(defrecord TestRecord [x y w h])

(def a (->TestRecord (+ 1 t) 3 "hello" :bob))

(defn update-values [m f]
  (let [transient-m (transient m)]
    (reduce (fn [acc [k v]]
              (assoc! acc k (f v)))
            transient-m
            m)
    (persistent! transient-m)))

(defn sample [x t]
  (update-values x #(clojure.core/+ % t)))

(sample a 5)
