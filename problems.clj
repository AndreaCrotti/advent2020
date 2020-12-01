(ns problems)

(defn read-input
  [n]
  (-> (format "%s.txt" n)
      slurp
      clojure.string/split-lines))

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
