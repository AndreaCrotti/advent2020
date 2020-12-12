(ns problems
  (:require [clojure.string :as str]
            [clojure.set :as s]
            [clojure.math.combinatorics :as c]
            [loom.graph :as lg]
            [loom.alg :as la]
            [utils :as u]))

(def p1 (map u/str->int (u/read-input 1)))

(defn p1-a []
  (first
   (for [a p1, b p1
         :when (= 2020 (+ a b))]
     (* a b))))

(defn p1-b []
  (first
   (for [a p1, b p1, c p1
         :when (= 2020 (+ a b c))]
     (* a b c))))

(def p2 (u/read-input 2))

(defn- parse-line [l]
  (let [parsed
        (->> l
             (re-find #"((\d+)-(\d+)) (\w): (\w+)")
             (drop 2)
             vec)]
    (-> parsed
        (update 0 u/str->int)
        (update 1 u/str->int)
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

(def p3 (u/read-input 3))

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

(def p4 (u/read-input 4))

(defn- get-fields [passport]
  (into {}
        (for [[k v] (map #(str/split % #":") passport)]
          [(keyword k) v])))

(defn- valid-passport? [passport-keys]
  (s/subset?
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
  (some-> s u/str->int))

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
  (let [h (u/str->int h)]
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

(def p5 (u/read-input 5))

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

(def p6
  (->> (u/read-input 6)
       (partition-by #(= "" %))
       (remove #(= [""] %))))

(defn yes-count [reduce-fn group]
  (->> group
       (map set)
       (reduce reduce-fn)
       count))

(defn p6* [reduce-fn]
  (->> p6
       (map (partial yes-count reduce-fn))
       (apply +)))

(defn p6-a [] (p6* clojure.set/union))
(defn p6-b [] (p6* clojure.set/intersection))

(def p7 (u/read-input 7))

(def zeroth #".*no other bags.*")
(def morth #"(.*) bags? contain (.*)")
(def inner-reg #"(\d+) (.*) bags?")

(defn parse-colour [c]
  (let [[_ n colour] (re-find inner-reg c)]
    [n colour]))

(defn parse-rule [r]
  (when-not (re-find zeroth r)
    (let [mm (re-find morth r)
          [_ big smalls] mm
          components (str/split smalls #", ")]
      (for [[n c] (map parse-colour components)]
        [c big (u/str->int n)]))))

(defn get-graph []
  (->> (map parse-rule p7)
       (remove nil?)
       (apply concat)
       (apply lg/weighted-digraph)))

(defn p7-a []
  (->> "shiny gold"
       (la/bf-traverse (get-graph))
       count
       ;; removing the node itself
       dec))

(defn n-bags-inside [gr starting]
  (let [edges (lg/out-edges gr starting)
        inner-edges (filter #(seq (lg/successors gr (second %))) edges)
        inner-weights (map (partial lg/weight gr) inner-edges)]
    (if (empty? edges)
      1
      (apply +
             (concat inner-weights
                     (for [e edges]
                       (* (lg/weight gr e)
                          (n-bags-inside gr (second e)))))))))

(defn p7-b []
  (n-bags-inside (lg/transpose (get-graph)) "shiny gold"))

(def p8 (u/read-input 8))

(defn parse-instr [i]
  (drop 1 (re-find #"(.*) (.*\d+)" i)))

(defn instr [instr current acc]
  (let [[op n] (parse-instr instr)
        n' (u/str->int n)]
    (condp = op
        "nop" [(inc current) acc]
        "acc" [(inc current) (+ acc n')]
        "jmp" [(+ current n') acc])))

;; need to keep track also at the instructions that were executed already
(defn vm [instructions]
  ;; current is the line number, acc the accumulator and evaluated
  ;; keep track if something was already executed or not
  (loop [current 0, acc 0, evaluated #{}]
    (if (or (= current (count instructions)) (contains? evaluated current))
      acc
      (let [[new-cur new-acc] (instr (nth instructions current)
                                     current acc)]
        (recur new-cur new-acc (conj evaluated current))))))

(def p8-a (partial vm p8))

(defn terminates? [instructions]
  ;; current is the line number, acc the accumulator and evaluated
  ;; keep track if something was already executed or not
  (loop [current 0, acc 0, evaluated #{}]
    (cond
      (contains? evaluated current) false
      (= current (count instructions)) true
      :else (let [[new-cur new-acc] (instr (nth instructions current)
                                           current acc)]
              (recur new-cur new-acc (conj evaluated current))))))

(defn possible-changes
  "Replace oee nop with one jmp or viceversa"
  [instructions]
  (loop [idx 0, changes []]
    (if (= idx (count instructions))
      changes
      (let [i (nth instructions idx)
            [op _n] (parse-instr i)
            replace (get {"jmp" "nop"
                          "nop" "jmp"} op)]
        (recur (inc idx)
               (if replace
                 (conj changes [replace idx])
                 changes))))))

(defn terminating-alternatives [instructions]
  ;;TODO: use loop/recur to avoid visiting all possible solutions
  ;;since we don't need to try them all
  (->> (for [[ch idx] (possible-changes instructions)]
         (let [[_op n] (parse-instr (nth instructions idx))
               new-instructions (assoc instructions idx (str ch " " n))]
           (when (terminates? new-instructions)
             new-instructions)))
       (filter some?)
       first))

(defn p8-b []
  (-> p8
      terminating-alternatives
      vm))

(def p9 (map u/str->int (u/read-input 9)))

(defn gen-subsets [numbers preamble-size]
  (for [idx (range preamble-size (count numbers))
        :let [cur (nth numbers idx)]]
    [cur (for [a (take preamble-size (drop (- idx preamble-size) numbers))
               b (take preamble-size (drop (- idx preamble-size) numbers))
               :when (not= a b)]
           [a b])]))

(defn is-sum? [n subsets]
  (contains?
   (->> subsets
        (map #(apply + %))
        set)
   n))

(defn intruder [numbers preamble-size]
  (->> (gen-subsets numbers preamble-size)
       (filter (fn [[n subs]] (not (is-sum? n subs))))
       ffirst))

(defn contiguous [numbers sum]
  (loop [size 2, idx 0]
    (let [inp-size (count numbers)
          sub (take size (drop idx numbers))]
      (if (= idx inp-size)
        (recur (inc size) 0)
        (when-not (= size inp-size)
          (if (= sum (apply + sub))
            (+ (apply min sub) (apply max sub))
            (recur size (inc idx))))))))

(defn p9-a [] (intruder p9 25))

(defn p9-b [] (contiguous p9 (p9-a)))

(def p10 (map u/str->int (u/read-input 10)))

(defn p10-a [])

(defn p10-b [])

(def ch->meaning
  {\L :free
   \. :floor
   \# :busy})

(def p11
  (->> (u/read-input 11)
       (mapv #(mapv ch->meaning %))))

(defn contained? [grid [x y]]
  (and (>= x 0) (>= y 0)
       (< x (count grid))
       (< y (count (nth grid x)))))

;;TODO: use this instead??
(defn incrs []
  (filter
   #(= 1 (Math/abs (apply + %)))
   (c/cartesian-product
    [-1 0 1]
    [-1 0 1])))

(defn adjacent [grid x y]
  (filter (partial contained? grid)
          [
           [(dec x) (dec y)]
           [(dec x) (inc y)]
           [(dec x) y]
           [(inc x) (dec y)]
           [(inc x) (inc y)]
           [(inc x) y]
           [x (dec y)]
           [x (inc y)]
           ]))

(defn cell-value [grid x y]
  (nth (nth grid x) y))

(defn set-value [grid [x y v]]
  (assoc-in grid [x y] v))

;; given a grid return a new grid applying the rules of evolution
(defn changes [grid]
  (for [x (range (count grid))
        y (range (count (first grid)))
        :let [cell (cell-value grid x y)
              adj (map #(apply cell-value (cons grid %))
                       (adjacent grid x y))]]

    (cond
      (= :free cell) (when-not (contains? (set adj) :busy) [x y :busy])
      (= :busy cell) (when (>= (count (filter #(= :free %) adj)) 4) [x y :free]))))

(defn evolve [grid]
  (loop [new-grid grid
         ch (changes grid)]
    (if (empty? ch)
      new-grid
      (if (nil? (first ch))
        (recur new-grid (rest ch))
        (recur (set-value new-grid (first ch))
               (rest ch))))))

(defn count-occupied [grid]
  (->> grid
       (map (fn [v]
              (->> v
                   (filter #(= :busy %))
                   count)))
       (apply +)))

(defn fixed-point [grid]
  (loop [next-grid (evolve grid)]
    (if (= next-grid grid)
      grid
      (recur (evolve grid)))))

(defn p11-a []
  (count-occupied
   (fixed-point p11)))

(defn p11-b [] )
