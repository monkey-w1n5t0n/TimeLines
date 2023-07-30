(ns timelines.operators
  (:require [clojure.spec.alpha :as s]))

(s/def ::range (s/and (s/cat :start number? :end number?)
                      #(<= (:start %) (:end %))))

(defprotocol P-Range
  (range [this])
  (range-at [this t])
  (range-at-segment [this {:keys [start end]}]))

(extend-protocol P-Range
  clojure.lang.PersistentVector
  ;; TODO @optimisation shouldn't have to travel the list twice
  (range [this] [(min this) (max this)])
  (range-at [this t]
    (let [sampled (mapv #(sample-at % t) this)]
      [(min sampled) (max sampled)])))

;; list of common arithmetic operators
(def operator-symbols
  '[+ - * / % sin cos tan asin acos atan])

;; too tricky and not useful enough
(def not-implemented [tan atan])

(def operators-range-id '[+ - * /])

(defn apply-sine-range [sig]
  (let [{:keys [min max] :as domain} (range sig)]
    (cond
      (< min (- half-pi) max)
      :unknown [-1 1])))

[0 (half pi)]

(defn sine-extremes [start end]
  (let [start-rem (mod (+ start (/ Math/PI 2)) Math/PI)
        end-rem (mod (+ end (/ Math/PI 2)) Math/PI)]
    (cond
      ;; if range includes an integer multiple of pi/2 (shifted by pi/2), both extremes are included
      (or (and (<= start-rem (/ Math/PI 2)) (>= end-rem (/ Math/PI 2)))
          (and (> start-rem (/ Math/PI 2)) (< end-rem (/ Math/PI 2))))
      '(-1 1)

      ;; if range starts and ends in the same half-period, no extremes are included
      (= (int (/ start-rem Math/PI)) (int (/ end-rem Math/PI)))
      (let [s (Math/sin start) e (Math/sin end)]
        (if (< s e) [s e] [e s]))

      ;; else, one extreme is included, depending on the half-period where the range starts
      :else
      (let [s (Math/sin start) e (Math/sin end)
            half-period-extreme (if (even? (int (/ start-rem Math/PI))) -1 1)]
        (if (< s e) [min s half-period-extreme max e half-period-extreme] [min e half-period-extreme max s half-period-extreme])))))

(sine-extremes 0 (+ 0.1 (/ Math/PI 2)))

(defn sine-min-max
  [[start end]]
  (let [periods-start (Math/floor (/ start (* 2 Math/PI)))
        periods-end (Math/ceil (/ end (* 2 Math/PI)))
        critical-points (concat (map #(* % (* 2 Math/PI)) (range periods-start periods-end))
                                [(+ (* periods-start (* 2 Math/PI)) Math/PI)
                                 (+ (* periods-start (* 2 Math/PI)) (/ Math/PI 2))
                                 (+ (* periods-end (* 2 Math/PI)) (/ Math/PI 2))
                                 (+ (* periods-end (* 2 Math/PI)) Math/PI)])
        in-range-points (filter #(and (>= % start) (<= % end)) critical-points)]
    (if (empty? in-range-points)
      (let [start-sin (Math/sin start)
            end-sin (Math/sin end)]
        [(min start-sin end-sin) (max start-sin end-sin)])
      [-1 1])))

(sine-min-max [0 1])

(Math/sin (- (/  Math/PI 2)))

(def operators-range-special
  {'sin (fn [x] (if) [:overwrite [-1 1]])
   'cos [:overwrite [-1 1]]
   'asin []})
