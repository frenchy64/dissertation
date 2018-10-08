(ns demo.isa-nil
  (:refer-clojure :exclude [fn defn])
  (:require [clojure.core.typed :refer [fn U]]))

(fn [x :- (U nil Number)]
  (if (isa? (class (class x)) Class) 
    (inc x) 0))
