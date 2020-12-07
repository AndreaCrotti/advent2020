(ns utils
  (:require [clojure.string :as str]))

(defn str->int [c] (Integer/parseInt c))

(defn read-input
  [n]
  (-> (format "%s.txt" n)
      slurp
      str/split-lines))
