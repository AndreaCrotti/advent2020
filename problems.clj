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
  [max-x inc-x inc-y]
  (loop [x' 0,
         y' 0,
         acc []]
    (if (> x' max-x)
      acc
      (recur (+ x' inc-x)
             (+ y' inc-y)
             (concat acc [[x' y']])))))

(defn is-tree? [forest [x y]]
  (= \# (-> forest
            (nth x)
            (nth y))))

(defn enlarge-forest [forest]
  ;;TODO: avoid this made up 100 number
  (map #(str/join (repeat 100 %)) forest))

(def p3-full (enlarge-forest p3))

(defn how-many-trees? [[inc-x inc-y]]
  (let [max-x  (count p3-full)
        coords (coordinates (dec max-x) inc-x inc-y)]

    (->> coords
         (map (partial is-tree? p3-full))
         (filter true?)
         count)))

(def slopes [[1 1]
             [1 3]
             [1 5]
             [1 7]
             [2 1]])

(comment
  (apply * (map how-many-trees? slopes)));; => 9406609920

(def p4 (read-input 4))

(defn- get-fields [passport]
  (into {}
        (map #(str/split % #":") passport)))

(defn- valid-passport? [passport-keys]
  (clojure.set/subset?
   #{"eyr" "pid" "byr" "ecl" "hcl" "hgt" "iyr"}
   passport-keys))

(def cleaned-p4
  (->> p4
       (partition-by #(= "" %))
       (remove #(= [""] %))
       (map #(str/join " " %))
       (map #(str/split % #" "))
       (map get-fields)
       (map (comp set keys))))

(comment
  (->> cleaned-p4
       (map valid-passport?)
       (filter true?)
       count))
;; => nil
;; => nil
