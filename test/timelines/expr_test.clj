(ns timelines.expr-test
  (:require [timelines.specs :refer :all]
            [timelines.expr :as e]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest testing is]]))

(def test-fns [{:e '(fn [t] t)
                :args '[t]
                :body '[t]
                :return-e 't}
               {:e '(fn [t] (+ 1 t))
                :args '[t]
                :body '[(+ 1 t)]
                :return-e '(+ 1 t)}
               {:e '(fn [t] (println "hi!")
                      (+ 1 t))
                :args '[t]
                :body '[(println "hi!")
                        (+ 1 t)]
                :return-e '(+ 1 t)}
               {:e '(fn [t x y] (write x y) (+ 1 t))
                :args '[t x y]
                :body '[(write x y) (+ 1 t)]
                :return-e '(+ 1 t)}])

(-> test-fns first :e e/fn-args)

(deftest is-fn?
  (doseq [[answer e] [[true '(fn [x] x)]
                      [true '(fn [x])]
                      [true '(fn [])]
                      [true '(clojure.core/fn [x] x)]
                      ;; Not sure if these should be supported...
                      ;; [true '(fn (x) x)]
                      ;; [true '[fn [x] x]]
                      ;; [true '[fn (x) x]]
                      [true '(fn [x y z hello] (println x) x)]
                      [true '(fn [x] (+ 1 (/ 3 x)))]
                      [true '(fn [x] ((fn [y]
                                        (* 2 y))
                                      (+ x 1)))]
                      ;; Falses
                      [false '(bob [x] x)]
                      [false '(fn x x)]
                      [false '(fn x ((fn [y]
                                       (* 2 y))
                                     (+ x 1)))]
                      [false '([x] (+ 1 x))]]]
    (is (= answer (e/fn? e)))))

(deftest fns
  (doseq [{:keys [e args body return-e]} test-fns]
    (is (= args (e/fn-args e)))
    (is (= body (e/fn-body e)))
    (is (= return-e (e/fn-return-e e)))))

(deftest premapping
  (doseq  [[e f result]
           '[[(fn [t] t)
              (+ 1)
              (fn [t] (-> t
                          (+ 1)
                          ((fn [t] t))))]
             [(fn [t] t)
              (fn [x] (+ x 1))
              (fn [t] (-> t
                          ((fn [x] (+ x 1)))
                          ((fn [t] t))))]]]
    (is (= result (e/sigfn-premap e f)))))

(deftest postmapping
  (doseq  [[e f result]
           '[[(fn [t] t)
              (+ 1)
              (fn [t] (-> t
                          ((fn [t] t))
                          (+ 1)))]
             [(fn [t] t)
              (fn [x] (+ x 1))
              (fn [t] (-> t
                          ((fn [t] t))
                          ((fn [x] (+ x 1)))))]]]
    (is (= result (e/sigfn-postmap e f)))))
