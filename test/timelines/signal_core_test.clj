(ns timelines.signal-core-test
  (:require [timelines.signal.core :refer :all]
            [clojure.test :refer :all]))


(deftest signal-making
  (testing "General `make-signal`"
    (is (->Signal 3 true)
        (make-signal 3))

    (is (->Signal '(fn [t] t) false)
        (make-signal '(fn [t] t)))

    (is (->Signal 5 true)
        (make-signal '(fn [t] 5)))
    ))

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
