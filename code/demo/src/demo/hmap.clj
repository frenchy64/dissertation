(ns demo.hmap
  (:refer-clojure :exclude [fn defn])
  (:require [clojure.core.typed :refer [defalias Num U fn ann-form defn Kw
                                        Any ann]]))

(defalias Expr
  (U '{:op ':if, :test Expr, :then Expr, :else Expr}
     '{:op ':do, :left Expr, :right Expr}
     '{:op ':const, :val Num}))

(defn an-exp [] :- Expr
  (let [v {:op :const, :val 1}]
    {:op :do, :left v, :right v}))

(ann dec-leaf [Expr -> Expr])
(defn dec-leaf [m]
  (if (= (:op m) :if)
    {:op :if, 
     :test (dec-leaf (:test m)),
     :then (dec-leaf (:then m)),
     :else (dec-leaf (:else m))}
    (if (= (:op m) :do)
      {:op :do,
       :left  (dec-leaf (:left m)),
       :right (dec-leaf (:right m))}
      {:op :const,
       :val (dec (:val m))})))
