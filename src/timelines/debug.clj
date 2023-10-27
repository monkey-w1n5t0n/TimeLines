(ns timelines.debug)

(def *dbg (atom false))

(defmacro dbg [expr]
  `(when @timelines.debug/*dbg
     ~expr))

(defn dbgprint [x]
  (dbg (println  x)))

(defmacro dbg-wrap [name expr]
  `(do
     (dbgprint (str "~~DBG: " name " START~~"))
     (let [result ~expr]
       (dbgprint result)
       (dbgprint (str "~~DBG: " name " END~~"))
       result)))

(comment
  (macroexpand-1 '(dbg (println "hi"))))
