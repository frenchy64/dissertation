(ns infer.core
  (:use clojure.test))

(defn nodes "Returns the number of nodes in the tree t."
  [t] (case (:op t)
        :node (+ 1 (nodes (:left t)) (nodes (:right t)))
        :leaf 1))
(defn visit-leaf "Updates :leaf nodes in tree t with function f."
  [f t] (case (:op t)
          :node (assoc t :left (visit-leaf f (:left t))
                         :right (visit-leaf f (:right t)))
          :leaf (f t)))
