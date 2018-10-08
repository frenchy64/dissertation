(ns demo.eg4
  (:require [clojure.core.typed :as t]))

;; HMap merging
(t/fn [m1 :- '{:a t/Num}
       m2 :- '{:b t/Num}]
  :- '{:a t/Num :b t/Num}
  (merge m1 m2))
; ^-----------^
;; Type mismatch:
;; 
;; Expected: 	(t/HMap :mandatory {:a t/Num, :b t/Num})
;; 
;; Actual: 	(t/HMap :mandatory {:b t/Num, :a t/Any})
