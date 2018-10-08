(ns demo.parent2
  (:require [clojure.core.typed :refer [ann All Str U defalias] :as t])
  (:import (java.io File)))

(defalias MFile '{:file (U nil File)})
(defalias MStr (U nil Str))
(defn parent [{f :file} :- MFile] :- MStr
  (if f (.getParent f) nil))
