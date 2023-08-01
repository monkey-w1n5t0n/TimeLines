(ns timelines.globals)

(def screen-height 600)
(def screen-width 800)

(def *main-canvas (atom nil))

(def *behaviors (atom {}))

(defonce *protocols (atom #{}))
