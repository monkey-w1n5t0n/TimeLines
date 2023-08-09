(ns timelines.globals)

(let [scale 1.5]
  (def screen-height (int (* scale 600)))
  (def screen-width  (int (* scale 800))))

(def *main-canvas (atom nil))

(def *behaviors (atom {}))

(defonce *protocols (atom #{}))
