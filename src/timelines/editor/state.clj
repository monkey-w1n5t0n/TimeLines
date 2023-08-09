(ns timelines.editor.state
  (:require [timelines.api :refer :all]))

(def BPM 10)
(def meter 4)

  ;; Beat duration
(def beatDur (/ 60 BPM))

  ;; Bar duration
(def barDur (* beatDur meter))

  ;; Timed phasors
(def beat
  (mod1 (/ t beatDur)))

(def bar
  (mod1 (/ t barDur)))

  ;; Timed phasor that goes between 0 and meter
(def beat-n
  (mod (/ t beatDur) meter))

  ;; Stepped counters
(def beatNum
  (int (/ t beatDur)))

(def barNum
  (int (/ t barDur)))
