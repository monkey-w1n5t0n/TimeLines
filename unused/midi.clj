(ns timelines.midi
  (:require [midi :as m]))


(comment
  (def keyboard (m/midi-in))

  (def novationXL (m/midi-in))
  (def novationXL2 (m/midi-in))


  (def xl2 (m/midi-in "Launch Control XL"))

  (for [s (m/midi-sources)]
    (println s))

  (m/midi-device? novationXL)
  (m/midi-device? novationXL2)

  (m/midi-handle-events novationXL
                        (fn [msg]
                          (println "hello")
                          (println msg)))

  (m/midi-handle-events xl2
                        (fn [msg] (println "hello"))))
