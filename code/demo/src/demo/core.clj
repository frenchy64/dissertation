(ns demo.core
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

;; Multimethod variant+map double dispatch
(t/defalias C (t/U '{:op ':if :test C :then C :else C}
                   '[':do C C]
                   '{:op ':val :val t/Any}))
(t/ann change [M E -> C])
(defmulti change (t/fn [m :- M, v :- E] 
                   [(:op m) (first v)]))
(defmethod change [:if :if] [{:keys [test then else]} [_ vtest vthen velse]]
  {:op :if
   :test (change test vtest)
   :then (change then vthen)
   :else (change else velse)})
(defmethod change [:do :do] [{:keys [e1 e2]} [_ ve1 ve2]]
  [:do (change e1 ve1) (change e2 ve2)])
(defmethod change [:val :val] [{:keys [val]} [_ vval]]
  (assert (= val vval))
  {:op :val :val val})

(change {:op :if 
         :test {:op :val :val 1}
         :then {:op :do 
                :e1 {:op :val :val 2} 
                :e2 {:op :val :val 3}}
         :else {:op :val :val 4}}
        [:if [:val 1] [:do [:val 2] [:val 3]] [:val 4]])
