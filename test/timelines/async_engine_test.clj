(ns timelines.async-engine-test
  (:require [timelines.async-engine :refer :all]
            [clojure.test :refer :all]
            [clojure.core.async :as a]))

(deftest test-create-node
  (let [node (create-node 'test-node '(+ 1 2))]
    (is (= (:name node) 'test-node))
    (is (= (:expr node) '(+ 1 2)))
    (is (instance? clojure.core.async.impl.channels.ManyToManyChannel (:chan node)))
    (is (nil? @(:mult node)))
    (is (nil? @(:value node)))
    (is (true? @(:dirty node)))
    (is (empty? @(:deps node)))))

(deftest test-update-deps!
  (initialize-graph!)
  (insert-node! 'a 1)
  (insert-node! 'b 2)
  (let [node (create-node 'c '(+ a b))]
    (update-deps! graph node)
    (is (= @(:deps node) #{'a 'b}))))

(deftest test-insert-node!
  (initialize-graph!)
  (insert-node! 'a 1)
  (insert-node! 'b '(+ a 1))
  (is (contains? @graph 'a))
  (is (contains? @graph 'b))
  (is (= @(:deps (get @graph 'b)) #{'a})))

(deftest test-evaluate-node
  (initialize-graph!)
  (insert-node! 'a 1)
  (insert-node! 'b 2)
  (insert-node! 'c '(+ a b))
  (is (= (evaluate-node graph 'a) 1))
  (is (= (evaluate-node graph 'b) 2))
  (is (nil? (evaluate-node graph 'c))) ; This will be nil initially
  (compute-node! graph 'a)
  (compute-node! graph 'b)
  (is (= (evaluate-node graph 'c) 3))) ; Now it should evaluate to 3

(deftest test-compute-node!
  (initialize-graph!)
  (insert-node! 'a 1)
  (insert-node! 'b 2)
  (insert-node! 'c '(+ a b))
  (compute-node! graph 'a)
  (compute-node! graph 'b)
  (let [result (compute-node! graph 'c)]
    (is (= result 3))
    (is (= @(:value (get @graph 'c)) 3))
    (is (false? @(:dirty (get @graph 'c))))))

(deftest test-mark-dirty!
  (initialize-graph!)
  (insert-node! 'a 1)
  (insert-node! 'b '(+ a 1))
  (insert-node! 'c '(+ b 1))
  (compute-node! graph 'c)
  (mark-dirty! graph 'a)
  (is (true? @(:dirty (get @graph 'a))))
  (is (true? @(:dirty (get @graph 'b))))
  (is (true? @(:dirty (get @graph 'c)))))

(deftest test-run-graph
  (initialize-graph!)
  (insert-node! 'a 1)
  (insert-node! 'b '(+ a 1))
  (insert-node! 'c '(+ b 1))
  (let [result (run-graph @graph 'c)]
    (is (= result 3))
    (is (= @(:value (get @graph 'a)) 1))
    (is (= @(:value (get @graph 'b)) 2))
    (is (= @(:value (get @graph 'c)) 3))))
