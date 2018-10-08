(ns demo.eg3
  (:require [clojure.core.typed :as t]))

;; let-aliasing with paths
(t/fn [{x :a :as m} :- '{:a (t/U nil t/Num)}]
  (let [x1 x]
    (when (number? (:a m))
      (inc x1))))
