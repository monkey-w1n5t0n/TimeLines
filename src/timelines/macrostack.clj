(ns timelines.macrostack)

(def *stack (atom []))

(defn expand [expr stack]
  (loop [expr expr
         stack stack]
    (if (empty? stack)
      expr
      (recur (apply (first stack) [expr])
             (rest stack)))))

(defn expand-at [expr stack t]
  (loop [expr expr
         stack stack]
    (if (empty? stack)
      expr
      (recur (apply (first stack) [(sample-at expr t)])
             (rest stack)))))

(defn expand-at-at [expr stack t]
  (loop [expr expr
         stack stack]
    (if (empty? stack)
      expr
      (recur (apply (sample-at (first stack) t)
                    [(sample-at expr t)])
             (rest stack)))))


(expand '(+ 1 2) [(fn [expr] (conj expr '?))
                  (fn [expr] (conj expr 999))
                  (fn [expr] (conj expr 123))])
