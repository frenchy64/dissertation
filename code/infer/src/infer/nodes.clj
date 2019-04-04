(ns infer.nodes
  (:require [clojure.core.typed :as t])
  (:use clojure.test))

(defn nodes "Returns the number of nodes in the tree t."
  [t] (case (:op t)
        :node (+ 1 (nodes (:left t)) (nodes (:right t)))
        :leaf 1))
