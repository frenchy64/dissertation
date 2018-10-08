(ns demo.atom
  (:refer-clojure :exclude [atom])
  (:require [clojure.core.typed :refer [atom Num]]))

(swap! (atom :- Num 1) + 2 3);=> 6 (atom contains 6)
