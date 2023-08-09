(ns timelines.api-test
  (:require [timelines.api :refer :all]
            [timelines.protocols :refer [sample-at]]
            [clojure.test :as t]))

(t/deftest fasting
  (t/is (= 2 (sample-at (fast 2 t) 1)))
  #_(is (= 3.0 (sample-at (fast 2 t) 1.5)))
  #_(is (= "hello" (sample-at (fast 2 "hello") 5/8))))
