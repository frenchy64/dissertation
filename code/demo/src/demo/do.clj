(ns demo.do
  (:refer-clojure :exclude [fn])
  (:require [clojure.core.typed :refer [fn U Num]]))

(fn [x :- (U nil Num)] 
  {:pre (number? x)}
  (inc x))

(fn [x :- (U nil Num)] 
  (assert (number? x))
  (inc x))

(fn [x :- (U nil Num)]
  (do (if (number? x) nil (throw (new Exception)))
      (inc x)))
