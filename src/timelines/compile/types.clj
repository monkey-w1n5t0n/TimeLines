(ns timelines.compile.types)


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
