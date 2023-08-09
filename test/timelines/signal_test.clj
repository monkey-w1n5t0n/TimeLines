(ns timelines.signal-test
  (:require [timelines.signal :as s]
            [timelines.signal :refer [defsig signal make-signal]]
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

#_(deftest postmapping
    (doseq [[op args result]
            [['clojure.core/+ '[1 2 3]
              (make-signal '(clojure.core/+ 1 2 3))]
             ['clojure.core/+ [1 2 (signal (fn [t] t))]
              (make-signal '(clojure.core/fn [t] (clojure.core/+ 1 2 (-> t ((fn [t] t))))))]
             ['clojure.core/+ [(make-signal '(fn [t] (+ 1 t)))
                               (make-signal '(fn [x] (/ x 2)))]
              (make-signal '(clojure.core/fn [t] (clojure.core/+ (-> t ((fn [t] (+ 1 t))))
                                                                 (-> t ((fn [x] (/ x 2)))))))]]]
      (is (= (:expr result) (:expr (s/apply-postmap op args))))))

#_(deftest premapping
    (doseq [[s f result]
            ['[1 (+ 1) 1]
             '["hello" (/ 8.5) "hello"]
             '[:hi (clojure.core/fn [x] (/ x 8.5)) :hi]
             [(make-signal '(clojure.core/fn [t] t))
              'clojure.core/inc
              (make-signal '(clojure.core/fn [t] (-> t clojure.core/inc ((clojure.core/fn [t] t)))))]
             [(make-signal '(clojure.core/fn [t] (/ t 2)))
              'clojure.core/inc
              (make-signal '(clojure.core/fn [t] (-> t clojure.core/inc ((clojure.core/fn [t] (/ t 2))))))]
             [(make-signal '(clojure.core/fn [t] (+ 1 t)))
              (make-signal '(clojure.core/fn [x] (/ x 2)))
              (make-signal '(clojure.core/fn [t] (-> t
                                                     ((clojure.core/fn [x] (/ x 2)))
                                                     ((clojure.core/fn [t] (+ 1 t))))))]]]
      (is (= result (s/apply-premap f s)))))

;; (deftest sampling
;;   (testing "Sampling signals, using both `sample-at-impl` and `sample-at`"
;;     (is (instance? timelines.signal.core/Signal))))
#_(comment
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
