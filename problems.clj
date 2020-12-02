(ns problems
  (:require [clojure.string :as str]))

(defn read-input
  [n]
  (-> (format "%s.txt" n)
      slurp
      str/split-lines))

(def p1 (map #(Integer/parseInt %) (read-input 1)))

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
  (->> l
       (re-find #"((\d+)-(\d+)) (\w): (\w+)")
       (drop 2)
       vec))

(defn valid-line-a? [l]
  (let [[from to ch password] (parse-line l)
        occ (get (frequencies password)
                 (first ch))]

    (and (some? occ)
         (<= (Integer/parseInt from) occ (Integer/parseInt to)))))

(count
 (filter valid-line-a? p2));; => 515

(defn valid-line-b? [l]
  (let [[from to ch password] (parse-line l)]
    (= [false true]
       (sort
        [(= (first ch) (nth password
                            (dec (Integer/parseInt from))))
         (= (first ch) (nth password
                            (dec (Integer/parseInt to))))]))))

(count
 (filter valid-line-b? p2));; => 711
