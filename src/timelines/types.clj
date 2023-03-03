(ns timelines.types
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

;; Args-List :: [Arg]
(defrecord Args-List [^Long num, list]
  P-Args-List
  (args-list [x] x))

(defrecord Type [^clojure.lang.Keyword kind])
(defrecord Type-List [^clojure.lang.PersistentVector types])

(defrecord Type-Signature [^Type-List in, ^Type-List out])

(defrecord Arg [^String name, ^Type type]
  P-Args-List
  (args-list [this]
    (Args-List. 1 (list this))))
