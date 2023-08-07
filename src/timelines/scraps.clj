(ns timelines.scraps)

;; From expr
(comment
;;;;
  ;; Macro idea for defn that supports opts for args
  ;; to automatically ensure them
  (comment
    (defmacro defn [& args]
      (let [{:keys [name args body opts]}
            (s/conform (s/cat :name simple-symbol?
                              :opts (s/* keyword?)
                              :args :clojure.expr.fn/arglist)
                       args)]
        `(~'defn ~name ~args ~@body)))

    (s/conform (s/cat :name simple-symbol?
                      :opts (s/* keyword?)
                      :args-and-opts (s/* N)
                      :body :clojure.expr.fn/body)
               '(foo [x y z] (+ x y z)))

    (s/conform (s/* (s/alt :sym symbol?
                           :sym+opt (s/cat :sym symbol? :opt keyword?)))
               '[x y :clojure.expr z])

    (defn time-arg [e :expr.fn/arglist y :expr/fn]
      (-> e parse-sigfn :args first))

    (macroexpand-1 '(defn test [x]
                      (-> x))))

  ;; automatic parsing ensurance, premature optimisation for now
  (comment

    (defn parse [spec e]
      (assoc
       (s/conform spec e)
       ::parsed? true
       ::type spec))

    (defn parsed? [e]
      (::type e))

    (defn enforce [spec e]
      (if (::parsed? e)
        e
        (let [conformed (s/conform spec e)]
          (if-not (= :clojure.spec.alpha/invalid conformed)
            conformed
            (do
              (s/explain spec e)
              (throw (Exception. "Spec mismatch, see above")))))))))
