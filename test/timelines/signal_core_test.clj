(ns timelines.signal-core-test
  (:require [timelines.signal.core :refer :all]
            [clojure.test :refer :all]))


(deftest signal-making
  (testing "General `make-signal`"
    (is (instance? timelines.signal.core/Signal s1))
    (is (instance? timelines.signal.core/Signal s2))

    (is (= s3-expr (:expr s3)))

    (is (->Signal  false)
        ())
    ))



(def test-signal-1 (signal (fn [t] t)))
(def test-signal-2 (signal 1))
(def test-signal-3 (signal (fn [t] (+ 1 t))))

;; (deftest sampling
;;   (testing "Sampling signals, using both `sample-at-impl` and `sample-at`"
;;     (is (instance? timelines.signal.core/Signal))))



(deftest signal-macro
  (testing "General `signal` macro expansion."
    (is (->Signal 3 true)
        (signal 3))
    (is (->Signal '(fn [t] t) false)
        (signal (fn [t] t)))
    ))

(deftest signal-macro-edges
  (testing "Edge cases of `signal` macro."
    (is (->Signal 5 true)
        (signal (fn [t] 5)))))
