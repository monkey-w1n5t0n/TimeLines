(ns timelines.reactive)





(defmacro defs-dyn [& args]
  (assert (even? (count args)))
  (let [pairs (loop [v args
                     acc []]
                (if
                    (empty? v) acc
                    (let [sym (first v)
                          val (second v)]
                      (recur (drop 2 v)
                             (conj acc
                                   `(def ~sym ~val)
                                   `(.setDynamic (var ~sym) true))))))]
    `(do ~@pairs)))

(defs-dyn
  t 123
  bar (* t 10)
  )

(def known-reactive-vars (agent {}))

(send known-reactive-vars assoc :test "hello")

(def expr  '(defmacro defs [& args]
             (assert (even? (count args)))
             (let [pairs (loop [v args
                                acc []]
                           (if
                               (empty? v) (:some-key {:hello "world"})
                               (recur (drop 2 v)
                                      (conj acc
                                            `(def ~(first v) ~(second v))))))]
               `(do ~@pairs))))

(defn flatten-map [m]
  (apply clojure.set/union (keys m) (vals m)))


(defn unique-symbols [expr]
  (loop [e expr
         acc #{}]
    (cond
      (empty? e) acc
      (symbol? e) (recur (next e)
                         (conj acc e))
      :else (recur (next e) acc))))


(loop [e '("hello" wowrld)
         acc #{}]
    (cond
      (empty? e) acc
      (symbol? e) (recur (next e)
                         (conj acc e))
      :else (recur (next e) acc)))




(defmacro def-reactive [sym & body]
  (let [depends-upon (filter )]))






(defs
  grid-num-rows 20
  grid-num-cols grid-num-rows

  grid-row-height (/ window-height grid-num-rows)
  grid-col-width  (/ window-width grid-num-cols)
  )




(defmacro dynamic)


dynamic

(defs-dyn
  bpm 100
  bps (/ bpm 60)
  meter-numerator 4
  meter-denomenator 4

  beat-dur (/ 60 bpm)
  bar-dur (* meter-numerator beat-dur)

  )



(binding [bpm 100]
  bpm)


(defs-dyn
  t 1
  )
