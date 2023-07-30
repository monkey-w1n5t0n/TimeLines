(ns timelines.base-api)

(defn from-list [lst phasor]
  (let [index (int (* phasor (count lst)))]
    (nth lst (min index (dec (count lst))))))
