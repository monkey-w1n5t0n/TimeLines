(ns timelines.draw.utils
  #_(:require
     ;; [clojure.pprint :refer [pprint]]
     ;; [clojure.string :as str]
     ;; [timelines.util.macros :refer [letmap pprint-macroexpand-1]]
     ;; [timelines.signal.core :refer :all]
     ;; [timelines.signal.api :refer [* +]]
     ;; [timelines.signal.api :refer :all]
     ;; [timelines.protocols :refer :all] ; [P-Samplable P-Drawable P-Samplable+Drawable sample-at]
     ;; [timelines.util.core :refer :all]
     ;; [timelines.util.core :as util]
     )
  (:import [org.lwjgl.glfw GLFW])

  )

(defn color [^long l]
  (.intValue (Long/valueOf l)))

;; TODO what's the type of window?
(defn display-scale [window]
  (let [x (make-array Float/TYPE 1)
        y (make-array Float/TYPE 1)]
    (GLFW/glfwGetWindowContentScale window x y)
    [(first x) (first y)]))
