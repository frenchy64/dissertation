(ns infer.visit-leaf
  (:require [clojure.core.typed :as t])
  (:use clojure.test))

;; Start: Generated by clojure.core.typed - DO NOT EDIT
(declare Op)
(t/defalias
  Op
  (t/U '{:op ':leaf, :val t/Int} '{:op ':node, :left Op, :right Op}))
(t/ann visit-leaf [[Op :-> Op] Op :-> Op])
;; End: Generated by clojure.core.typed - DO NOT EDIT
(defn visit-leaf "Updates :leaf nodes in tree t with function f."
  [f t] (case (:op t)
          :node (assoc t :left (visit-leaf f (:left t))
                         :right (visit-leaf f (:right t)))
          :leaf (f t)))