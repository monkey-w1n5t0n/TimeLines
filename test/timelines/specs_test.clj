(ns timelines.specs-test
  (:require [timelines.specs :refer :all]
            [clojure.spec.alpha :as s]
            [timelines.api :as api]

            [clojure.test :refer [deftest testing is]]))

(defmacro valid? [spec x]
  `(clojure.test/is (s/valid? ~spec ~x)))

(deftest clojure-base-specs
  (testing "Atomic types"
    (valid? :clojure/map {:hello "world" "Hi!" 123})))

(def simple-sig ())

(deftest signal-exprs
  (testing "Sigfuncs"
    (valid? ::sig-func '(fn [t] t))
    (is (and (s/valid? ::sig-func)))))
