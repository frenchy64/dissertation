(ns demo.java
  (:refer-clojure :exclude [])
  (:require [clojure.core.typed :refer [HMap]]))

(defn foo [m :- '{:file (U nil File)}]
