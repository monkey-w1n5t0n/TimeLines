(ns timelines.util.macros)

;; TODO refactor
(defmacro defs [& args]
  (assert (even? (count args)))
  (let [pairs (loop [v args
                     acc []]
                (if
                    (empty? v) acc
                    (recur (drop 2 v)
                           (conj acc
                                 `(def ~(first v) ~(second v))))))]
    `(do ~@pairs)))

(defmacro letmap [& args]
  (let [symbols (->> args (apply hash-map) keys)
        map-pairs (interleave (map keyword symbols) symbols)]
    `(let ~(into [] args)
       (hash-map ~@map-pairs))))
