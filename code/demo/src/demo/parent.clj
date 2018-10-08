(ns demo.parent
  (:require [clojure.core.typed :refer [ann U]])
  (:import (java.io File)))

(ann parent [(U nil File) -> (U nil File)])
(defn parent [^File f]
  (when f
    (let [s (.getParent f)]
      (assert s "Expected parent string, got nil")
      (File. s))))


(parent (File. "a/a"))
