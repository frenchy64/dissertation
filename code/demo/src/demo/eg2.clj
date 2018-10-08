(ns demo.eg2
  (:require [clojure.core.typed :as t]))

;; Simple let-aliasing
(t/fn [x :- (t/U nil t/Num)]
  (let [x1 x]
    (when (number? x)
      (inc x1))))
