(ns infer.nodes-test
  (:require [clojure.test :refer :all]
            [infer.nodes :refer :all]))

(def t1 {:op :node, :left {:op :leaf, :val 1}, :right {:op :leaf, :val 2}})
(deftest nodes-test (is (= (nodes t1) 3)))
