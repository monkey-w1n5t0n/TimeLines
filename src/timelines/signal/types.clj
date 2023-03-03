(ns timelines.signal.types
  (:require [timelines.protocols :refer :all]))


;; derivations
;; (derive Signal ::Signal)
;; (derive ::Static-Signal ::Signal)

(def static-signal-types
  [Number
   Long
   Integer
   clojure.lang.Ratio
   Double
   Float
   String
   clojure.lang.Keyword
   clojure.lang.Symbol
   clojure.lang.PersistentList
   clojure.lang.PersistentVector])

(doseq [type static-signal-types]
  (derive type ::Static-Signal))

