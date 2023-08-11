;; taken from https://github.com/HumbleUI/HumbleUI
(ns timelines.typography
  (:require [clojure.java.io :as io])
  (:import
   [java.io Writer]
   [io.github.humbleui.skija Data Font Typeface]))

(defonce *typeface-cache (atom {}))

(def font-name->resource-path
  {"FiraCode Regular" "resources/fonts/FiraCode-Regular.ttf"})

;; from humbleui
(do
  (def *default
    (delay
      (Typeface/makeDefault)))

  (defn typeface-from-data
    (^Typeface [^Data data]
     (Typeface/makeFromFile data 0))
    (^Typeface [^Data data index]
     (Typeface/makeFromFile data index)))

  (defn typeface-from-path
    (^Typeface [^String path]
     (Typeface/makeFromFile path 0))
    (^Typeface [^String path index]
     (Typeface/makeFromFile path index)))

  (defn typeface-from-name [name]
    (let [path (get font-name->resource-path name)]
      (if path
        (typeface-from-path path)
        (throw (Exception. (str "resource path for typeface " name "not found"))))))

  (defn typeface-from-resource
    (^Typeface [res]
     (typeface-from-resource res 0))
    (^Typeface [res index]
     (with-open [is (io/input-stream (io/resource res))]
       (let [bytes (.readAllBytes is)]
         (with-open [data (Data/makeFromBytes bytes)]
           (Typeface/makeFromData data index))))))

  (defn family-name ^String [^Typeface typeface]
    (.getFamilyName typeface))

  (defmethod print-method Typeface [o ^Writer w]
    (.write w "#Typeface{familyName=")
    (.write w (family-name o))
    (.write w "}"))

  (defn make-with-size ^Font [^Typeface typeface size]
    (Font. typeface (float size)))

  (defn make-with-cap-height ^Font [^Typeface typeface cap-height]
    (let [size    (float 100)
          font    (Font. typeface size)
          current (-> font .getMetrics .getCapHeight)
          size'   (-> size (/ current) (* cap-height))]
      (.setSize font size')))

  (defn typeface ^Typeface [^Font font]
    (.getTypeface font))

  (defn size [^Font font]
    (.getSize font))

  (defn metrics [^Font font]
    (let [m (.getMetrics font)]
      {:top                 (.getTop m)
       :ascent              (.getAscent m)
       :descent             (.getDescent m)
       :bottom              (.getBottom m)
       :leading             (.getLeading m)
       :avg-char-width      (.getAvgCharWidth m)
       :max-char-width      (.getMaxCharWidth m)
       :x-min               (.getXMin m)
       :x-max               (.getXMax m)
       :x-height            (.getXHeight m)
       :cap-height          (.getCapHeight m)
       :underline-thickness (.getUnderlineThickness m)
       :underline-position  (.getUnderlinePosition m)
       :strikeout-thickness (.getStrikeoutThickness m)
       :strikeout-position  (.getStrikeoutPosition m)
       :height              (.getHeight m)}))

  (defn set-size! [^Font font size]
    (.setSize font size))

  (defmethod print-method Font [o ^Writer w]
    (.write w "#Font{familyName=")
    (.write w (family-name (typeface o)))
    (.write w ", size=")
    (.write w (str (size o)))
    (.write w "}")))

#_(defn load-typeface [path]
    (with-open [is (io/input-stream (io/resource path))]
      (let [bytes (.readAllBytes is)]
        (with-open [data (Data/makeFromBytes bytes)]
          (Typeface/makeFromData data)))))

(defn get-typeface [name]
  (when-not (contains? @*typeface-cache name)
    (swap! *typeface-cache assoc name
           (typeface-from-name name)))
  (@*typeface-cache name))

