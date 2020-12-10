(ns problems-test
  (:require [clojure.test :refer [deftest is]]
            [problems :as p]
            [loom.graph :as lg]))

(deftest p1-test
  (is (= 980499 (p/p1-a)))
  (is (= 200637446 (p/p1-b))))

(deftest p2-test
  (is (= 515 (p/p2-a)))
  (is (= 711 (p/p2-b))))

(deftest p3-test
  (is (= 9406609920 (p/p3-b))))

(deftest p4-test
  (is (= 245 (p/p4-a)))
  (is (= 133 (p/p4-b))))

(deftest p5-test
  (is (= 44 (p/get-row "FBFBBFF")))
  (is (= 5 (p/get-column "RLR")))
  (is (= (p/find-seat "FBFBBFFRLR") [44 5]))
  (is (= 850 (p/p5-a)))
  (is (= 599 (p/p5-b))))

(deftest p6-test
  (is (= 6443 (p/p6-a)))
  (is (= 3232 (p/p6-b))))

(def sample-g
  (lg/weighted-digraph
   [:a :b 2]
   [:a :d 10]
   [:b :c 3]))

(def sample-long
  (lg/weighted-digraph
   [:sg :dr 2]
   [:dr :do 2]
   [:do :dy 2]
   [:dy :dg 2]
   [:dg :db 2]
   [:db :dv 2]))

(deftest p7-test
  (is (= [["drab silver" "striped white" 4]]
         (p/parse-rule "striped white bags contain 4 drab silver bags.")))

  (is (= nil
         (p/parse-rule "drab silver bags contain no other bags.")))

  (is (= [["muted silver" "plaid beige" 3]
          ["vibrant orange" "plaid beige" 4]]
         (p/parse-rule "plaid beige bags contain 3 muted silver bags, 4 vibrant orange bags.")))

  (is (= 103 (p/p7-a)))

  (is (= 18 (p/n-bags-inside sample-g :a)))
  (is (= 126 (p/n-bags-inside sample-long :sg)))
  (is (= 1469 (p/p7-b))))

(def input-test
  ["nop +0"
   "acc +1"
   "jmp +4"
   "acc +3"
   "jmp -3"
   "acc -99"
   "acc +1"
   "jmp -4"
   "acc +6"])

(deftest p8-test
  (is (= 5 (p/vm input-test)))
  (is (= 1723 (p/p8-a)))
  (is (false? (p/terminates? input-test)))
  (is (true? (p/terminates? (assoc input-test 7 "nop -4"))))

  (is (= [["jmp" 0] ["nop" 2] ["nop" 4] ["nop" 7]] (p/possible-changes input-test)))
  (is (= 846 (p/p8-b))))

(def p9-sample
  [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(deftest p9-test
  (is (= 127 (p/intruder p9-sample 5)))
  (is (= 258585477 (p/p9-a)))

  (is (= 36981213 (p/p9-b)))
  (is (= 62 (p/contiguous p9-sample 127))))

(def short-input [16 10 15 5 1 11 7 19 6 12 4])

#_(deftest p10-test
  (is (= p/jolts short-input [[7 1] [5 3]])))
