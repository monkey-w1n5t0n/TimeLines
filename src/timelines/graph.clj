(ns timelines.graph)

(defn make-graph [f sr w h]
  (let [
        time-domain (range w)
        half-h (/ h 2)
        range ((- half-h) half-h)

        image (q/create-image w h :rgb)
        vs (mapv f time-domain)
        ]
    (dotimes [x w]
      (doseq [y vs]
        (q/set-pixel image x y (q/color 0 0 0))
        ))
    image))
