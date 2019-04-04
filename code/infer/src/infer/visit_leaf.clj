(ns infer.visit-leaf
  (:require [clojure.core.typed :as t])
  (:use clojure.test))

(defn visit-leaf "Updates :leaf nodes in tree t with function f."
  [f t] (case (:op t)
          :node (assoc t :left (visit-leaf f (:left t))
                         :right (visit-leaf f (:right t)))
          :leaf (f t)))
