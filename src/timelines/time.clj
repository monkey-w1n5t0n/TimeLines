(ns timelines.time)

(defn millis []
  (System/currentTimeMillis))

(defn seconds []
  (/ (millis) 1000))

(def now seconds)
