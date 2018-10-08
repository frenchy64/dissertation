(ns demo.parent3
  (:refer-clojure :exclude [defn fn])
  (:require [clojure.core.typed :refer [ann All Str U fn defn] :as t])
  (:import (java.io File)))

(ann parent [(U nil File) -> (U nil Str)])
(defn parent [f]
  (if f (.getParent f) nil))

(fn [^File f :- File] (.getParent f))

(fn [^File f :- File] :- (U nil Str)
  (.getParent f))

(parent (new File s))


(defn parent [f :- (U nil File)]
  (if f (.getParent f) nil))
