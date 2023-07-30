(ns timelines.maths)

;; MATH
(def pi java.lang.Math/PI)
(def two-pi (* 2 pi))

(defn sine [t]
  (java.lang.Math/sin t))

(defn cos [t]
  (java.lang.Math/cos t))

(defn wrap01 [s]
  (mod s 1.0))
