(ns timelines.playground)

(def melody2
  (-> (* 2 pi t)
      sqr
      (slow 2)
      (fromList [1 2 3 4])
      m-to-f))

(def melody
  (>- m-to-f
      (fromList [1 2 3 4])
      (slow 2)
      sqr
      (* 2 pi t)))


(def melody | m-to-f | fromList [1 2 3 4] | slow 2 | sqr | (* 2 pi t))
