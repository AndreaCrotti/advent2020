(ns problems
  (:require [clojure.string :as str]))

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

(assert (= 980499 (p1-a)))

(defn p1-b []
  (->> (for [a p1, b p1, c p1]
         (when (= 2020 (+ a b c))
           (* a b c)))
       (filter some?)
       first))

(assert (= 200637446 (p1-b)))

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

(assert (= 515 (p2-a)))

(defn valid-line-b? [l]
  (let [[from to ch password] (parse-line l)]
    (= [false true]
       (sort
        [(= ch (nth password (dec from)))
         (= ch (nth password (dec to)))]))))

(defn p2-b []
  (count (filter valid-line-b? p2)))

(assert (= 711 (p2-b)))

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

(assert (= 245 (p4-a)))

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

(assert (= 133 (p4-b)))
