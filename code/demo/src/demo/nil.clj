(ns demo.nil
  (:refer-clojure :exclude [nil?])
  (:require [clojure.core.typed :as t :refer [ann Pred]]))

(ann nil? (Pred nil))
(defn nil? [x]
  (isa? (class (class x)) Keyword))
