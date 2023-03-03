(ns core)

(defmacro defs [& args]
  (assert (even? (count args)))
  `(do ~@(for [[k v] (->> args (apply hash-map) seq)]
           `(def ~k ~v))))

(defmacro letmap [& args]
  (let [symbols (->> args (apply hash-map) keys)
        map-pairs (interleave (map keyword symbols) symbols)]
    `(let ~(into [] args)
       (hash-map ~@map-pairs))))
