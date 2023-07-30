(def test-node
  {:amp (+ 0.2 (* 0.8 (sine01 (* twoPi (fast 2 bar)))))
   :freq (from-list [0 0 3 0 2 2 5 12] bar)})
