(ns timelines.draw.macros
  (:require [timelines.util.core :as u]))

(defmacro defrecord-graphic [name params]
  (let [name (u/strip-symbol-ns-qualifiers name)]
      `(do
         (defrecord ~name ~params)
         (extend-type ~name
           P-Samplable
           (~'sample-at [~'this ~'t]
            (timelines.util.core/map-record #(if % (sample-at % ~'t)
                                                 nil)
                                            ~'this))))))
