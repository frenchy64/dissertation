(ns ^:core.typed demo.eg-intro
  (:import (java.io File))
  (:require [clojure.core.typed :refer [ann Any U Str defalias]]))
;; a function annotation for `pname` multimethod.
;; Input: non-nil (null) File or String, via union, Ouput: nilable String
;(ann pname [(U File String) -> (U nil String)])
;(defmulti pname class) ; multimethod on arg's class
; ; String implementation, JVM constructors non-nil
;(defmethod pname String [s] (pname (new File s)))
;(defmethod pname File [f] (.getName f)) ; File implementation 
;; JVM method target `f` verified non-nil, but return is nilable
;(pname "STAINS/JELLY") ; :- (U nil Str)
;;=> "JELLY"

(ann pname [(U File String) -> (U nil String)])
(defmulti pname class)  ; multimethod dispatching on class of argument
(defmethod pname String [s] (pname (new File s))) ; String case 
(defmethod pname File [f] (.getName f)) ; File case, with static null check
(pname "STAINS/JELLY") ;=> "JELLY" :- (U nil Str)
