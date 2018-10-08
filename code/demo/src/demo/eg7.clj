(ns demo.eg7
  (:refer-clojure :exclude [fn])
  (:import (java.io File))
  (:require [clojure.core.typed :as t :refer [defalias ann fn Assoc Num U Kw Str]]))

(def hi {:en "hi" :fr "bonjour"})

(ann say [Any Any -> Str])
(defmulti say (fn [l a] [f (class a)]))
(defmethod say [:en String] [_ p]
  (str "Hi, " p "!"))
(defmethod say [:fr String] [_ p]
  (str "Bonjour, " (say :fr (count p)) "," p "!"))
(defmethod say [:fr Number] [_ i]
  (apply str
    (interpose ", " (repeat i "bonjour"))))
(defmethod say :default [l _] "Sorry ...")

(say :en "Grace")       ;=> "Hi, Grace!"
(say :fr "Donald")      ;=> "bonjour, bonjour!"
(say :fr 2)             ;=> "bonjour, bonjour!"
(say :en nil)           ;=> "Sorry ..."
