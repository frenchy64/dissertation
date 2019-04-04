(ns infer.core-test
  (:require [clojure.test :refer :all]
            [infer.core :refer :all]))

(def t1 {:op :node, :left {:op :leaf, :val 1}, :right {:op :leaf, :val 2}})
(deftest nodes-test
  (is (= (nodes t1) 3)))
(deftest visit-leaf-test
  (is (= (visit-leaf (fn [leaf] (assoc leaf :val (inc (:val leaf)))) t1)
         {:op :node, :left {:op :leaf, :val 2}, :right {:op :leaf, :val 3}})))
