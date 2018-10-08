(ns demo.eg6
  (:require [clojure.core.typed :as t]))

;; Multimethod Variant identity
(t/defalias E (t/U '[':if E E E]
                   '[':do E E]
                   '[':val t/Any]))
(t/ann variant [E -> E])
(defmulti variant first)
(defmethod variant :if [[_ test then else]]
  [:if test then else])
(defmethod variant :do [[_ e1 e2]]
  [:do e1 e2])
(defmethod variant :val [[_ v]]
  [:val v])

(variant [:if [:val 1] [:do [:val 2] [:val 3]] [:val 4]])
