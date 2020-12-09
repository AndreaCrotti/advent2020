(ns utils
  (:require [clojure.string :as str]))

(defn str->int [c] (Long/parseLong c))

(defn read-input
  [n]
  (-> (format "resources/%s.txt" n)
      slurp
      str/split-lines))
