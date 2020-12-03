(ns problems
  (:require [clojure.string :as str]))

(defn ch->int [c] (Integer/parseInt c))

(defn read-input
  [n]
  (-> (format "%s.txt" n)
      slurp
      str/split-lines))

(def p1 (map ch->int (read-input 1)))

(defn p1-sol-a []
  (doseq [a p1
          b p1]
    (when (= 2020 (+ a b))
      (println a, b, (* a b)))))

(defn p1-sol-b []
  (doseq [a p1
          b p1
          c p1]
    (when (= 2020 (+ a b c))
      (println a, b, c (* a b c)))))

(def p2 (read-input 2))

(defn parse-line [l]
  (let [parsed
        (->> l
             (re-find #"((\d+)-(\d+)) (\w): (\w+)")
             (drop 2)
             vec)]
    (-> parsed
        (update 0 ch->int)
        (update 1 ch->int)
        (update 2 first))))

(defn valid-line-a? [l]
  (let [[from to ch password] (parse-line l)
        occ (get (frequencies password) ch)]

    (and (some? occ)
         (<= from occ to))))

(count
 (filter valid-line-a? p2));; => 515

(defn valid-line-b? [l]
  (let [[from to ch password] (parse-line l)]
    (= [false true]
       (sort
        [(= ch (nth password (dec from)))
         (= ch (nth password (dec to)))]))))

(count
 (filter valid-line-b? p2));; => 711

(def p3 (read-input 3))

(defn coordinates
  "lazy sequences of coordinates to go through"
  [x y max-y]
  (loop [x' x,
         y' y,
         acc []]
    (if (> y' max-y)
      acc
      (recur (inc x')
             (+ y' 3)
             (concat acc [[x' y']])))))

(defn is-tree? [forest x y]
  (= \# (-> forest
            (nth x)
            (nth y))))
