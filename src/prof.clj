(ns prof
  (:require [clj-async-profiler.core :as prof]))

(prof/profile (dotimes [i 100000]
                (reduce + (range i))))
