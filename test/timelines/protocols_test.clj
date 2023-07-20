(ns timelines.protocols-test
  (:require [timelines.protocols :refer :all]
            [timelines.signal.core :only [Signal]]
            [timelines.examples :as e]
            [clojure.test :refer :all]))


(deftest symbolic-expressions
  (testing "Expressions of all common datatypes"
    ;; Static datatypes
    (is (= 3 (->expr 3)))
    (is (= 3.5 (->expr 3.5)))
    (is (= (/ 1 2) (->expr (/ 1 2))))
    (is (= "hello" (->expr "hello")))
    (is (= :hello (->expr :hello)))
    (is (= 'hello (->expr 'hello)))
    (is (= (list 1 2 3) (->expr (list 1 2 3))))
    (is (= [1 2 3] (->expr [1 2 3])))

    (is (= e/sig-1-expr (->expr e/sig-1)))
    ))


(deftest symbolic-expressions
  (testing "Expressions of all common datatypes"
    (is (= 3 (->expr 3)))
    (is (= 3.5 (->expr 3.5)))
    (is (= (/ 1 2) (->expr (/ 1 2))))
    (is (= "hello" (->expr "hello")))
    (is (= :hello (->expr :hello)))
    (is (= 'hello (->expr 'hello)))
    (is (= (list 1 2 3) (->expr (list 1 2 3))))
    (is (= [1 2 3] (->expr [1 2 3])))))
