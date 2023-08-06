(ns timelines.signal-test
  (:require [timelines.signal :refer :all]
            [timelines.api :as api]
            [timelines.protocols :refer [sample-at]]
            [clojure.test :refer :all]))

(defsig t1 (fn [t] t))
(def t2 (signal (fn [t] t)))
(def t3  (make-signal '(fn [t] t)))

(deftest sampling
  (doseq [t [api/t t1 t2 t3]]
    (is (= 5 (sample-at t 5)))
    (is (= (/ 5 8) (sample-at t (/ 5 8)))))

  (is (= 3 (sample-at (signal (fn [t] (+ 1 t))) 2)))

  (is (= "hi" (sample-at "hi" 123)))
  (is (= :hello (sample-at :hello -123)))
  (is (= [1 2 3] (sample-at [1 2 3] 123)))
  (is (= '(1 2 3) (sample-at [1 2 3] 123)))
  (is (= {:hi "hello" :world (/ -5 8)}
         (sample-at {:hi "hello" :world api/t}
                    (/ -5 8)))))

(let [exprs ['[(fn [t] t) (+ 1) (fn [t] (+ 1 t))]
             '[(fn [t] t) (+ 2.5) (fn [t] (+ 2.5 t))]
             '[(fn [t] t) (/ (/ 1 3)) (fn [t] (/ (/ 1 3)
                                                 t))]]]
  (deftest premap
    (doseq [[sig-expr postmap-expr result] exprs]
      (is (= result (expr/postmap postmap-expr sig-expr))))))

;; (deftest sampling
;;   (testing "Sampling signals, using both `sample-at-impl` and `sample-at`"
;;     (is (instance? timelines.signal.core/Signal))))
(comment
  (deftest signal-macro
    (testing "General `signal` macro expansion."
      (is (->Signal 3 true)
          (signal 3))
      (is (->Signal '(fn [t] t) false)
          (signal (fn [t] t)))))

  (deftest signal-macro-edges
    (testing "Edge cases of `signal` macro."
      (is (->Signal 5 true)
          (signal (fn [t] 5))))))
