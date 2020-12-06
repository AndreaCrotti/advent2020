(ns problems
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn str->int [c] (Integer/parseInt c))

(defn read-input
  [n]
  (-> (format "%s.txt" n)
      slurp
      str/split-lines))

(def p1 (map str->int (read-input 1)))

(defn p1-a []
  (->> (for [a p1, b p1]
         (when (= 2020 (+ a b))
           (* a b)))
       (filter some?)
       first))

(defn p1-b []
  (->> (for [a p1, b p1, c p1]
         (when (= 2020 (+ a b c))
           (* a b c)))
       (filter some?)
       first))

(deftest p1-test
  (is (= 980499 (p1-a)))
  (is (= 200637446 (p1-b))))

(def p2 (read-input 2))

(defn- parse-line [l]
  (let [parsed
        (->> l
             (re-find #"((\d+)-(\d+)) (\w): (\w+)")
             (drop 2)
             vec)]
    (-> parsed
        (update 0 str->int)
        (update 1 str->int)
        (update 2 first))))

(defn valid-line-a? [l]
  (let [[from to ch password] (parse-line l)
        occ (get (frequencies password) ch)]

    (and (some? occ)
         (<= from occ to))))

(defn p2-a []
  (count (filter valid-line-a? p2)))

(defn valid-line-b? [l]
  (let [[from to ch password] (parse-line l)]
    (= [false true]
       (sort
        [(= ch (nth password (dec from)))
         (= ch (nth password (dec to)))]))))

(defn p2-b []
  (count (filter valid-line-b? p2)))

(deftest p2-test
  (is (= 515 (p2-a)))
  (is (= 711 (p2-b))))

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

(defn p3-b []
  (apply * (map how-many-trees? slopes)))

(deftest p3-test
  (is (= 9406609920 (p3-b))))

(def p4 (read-input 4))

(defn- get-fields [passport]
  (into {}
        (for [[k v] (map #(str/split % #":") passport)]
          [(keyword k) v])))

(defn- valid-passport? [passport-keys]
  (clojure.set/subset?
   #{:eyr :pid :byr :ecl :hcl :hgt :iyr}
   passport-keys))

(def cleaned-p4
  (->> p4
       (partition-by #(= "" %))
       (remove #(= [""] %))
       (map #(str/join " " %))
       (map #(str/split % #" "))
       (map get-fields)))

(defn p4-a []
  (->> cleaned-p4
       (map valid-passport?)
       (filter true?)
       count))

(defn str->int-some [s]
  (some-> s str->int))

(defn matches [reg s]
  (some->> s (re-matches reg)))

(def passport-parse
  {:byr str->int-some
   :iyr str->int-some
   :eyr str->int-some
   :hcl #(matches #"#(\d|[abcdef]){6}" %)
   :hgt #(matches #"(\d+)(cm|in)" %)
   :ecl #(matches #"amb|blu|brn|gry|grn|hzl|oth" %)
   :pid #(matches #"\d{9}" %)})

(defn height [[_full h unit]]
  (let [h (str->int h)]
    (cond
      (= unit "cm") (<= 150 h 193)
      (= unit "in") (<= 59 h 76))))

(def passport-validate
  {:byr #(<= 1920 % 2002)
   :iyr #(<= 2010 % 2020)
   :eyr #(<= 2020 % 2030)
   :hgt height})

(def with-nil-support
  (into {}
        (for [[k f] passport-validate]
          [k (fn [v] (and (some? v) (f v)))])))

(defn valid-passport-2 [passport]
  (let [parsed (reduce-kv update passport passport-parse)
        valid (reduce-kv update parsed with-nil-support)]
    (and (valid-passport? (set (keys passport)))
         (->> valid
              vals
              ;;TODO: improve it here by simplifying the validation step
              (every? #(and (some? %) (not (false? %))))))))

(defn p4-b []
  (->> cleaned-p4
       (map valid-passport-2)
       (remove false?)
       count))

(deftest p4-test
  (is (= 245 (p4-a)))
  (is (= 133 (p4-b))))

(def p5 (read-input 5))

(defn d->pow [mapping idx item]
  (Math/round
   (* (get mapping item)
      (Math/pow 2 idx))))

(defn s->int [mapping s]
  (->> s
       reverse
       (map-indexed (partial d->pow mapping))
       (apply +)))

(def get-row (partial s->int {\F 0, \B 1}))
(def get-column (partial s->int {\L 0, \R 1}))

(defn find-seat [s]
  (let [[rs cs] (split-at 7 s)]
    [(get-row rs) (get-column cs)]))

(defn seat-id [s]
  (let [[r c] (find-seat s)]
    (+ c (* 8 r))))

(defn p5-a []
  (apply max (map seat-id p5)))

(defn p5-b []
  (let [all-seat-ids (sort (map seat-id p5))]
    (->> (for [[b a] (zipmap all-seat-ids (drop 1 all-seat-ids))]
           (let [d (- a b)]
             (when (> d 1)
               (inc b))))
         (remove nil?)
         first)))

(deftest p5-test
  (is (= 44 (get-row "FBFBBFF")))
  (is (= 5 (get-column "RLR")))
  (is (= (find-seat "FBFBBFFRLR") [44 5]))
  (is (= 850 (p5-a)))
  (is (= 599 (p5-b))))

(def p6
  (->> (read-input 6)
       (partition-by #(= "" %))
       (remove #(= [""] %))))

(defn yes-count [reduce-fn group]
  (->> group
       (map set)
       (reduce reduce-fn)
       count))

(defn p6* [reduce-fn]
  (apply +
         (map (partial yes-count reduce-fn) p6)))

(defn p6-a [] (p6* clojure.set/union))
(defn p6-b [] (p6* clojure.set/intersection))

(deftest p6-test
  (is (= 6443 (p6-a)))
  (is (= 3232 (p6-b))))
