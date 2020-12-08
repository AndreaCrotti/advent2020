(ns test-problems
  (:require [clojure.test :refer [deftest is]]
            [problems :as p]))

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

(deftest p7-test
  (is (= {"striped white" {"drab silver" 4}}
         (p/parse-rule "striped white bags contain 4 drab silver bags.")))

  (is (= {"drab silver bags contain no other bags." 0}
         (p/parse-rule "drab silver bags contain no other bags.")))

  (is (= {"plaid beige" {"muted silver" 3, "vibrant orange" 4}}
         (p/parse-rule "plaid beige bags contain 3 muted silver bags, 4 vibrant orange bags.")))

  (is (= 103 (p/p7-a))))

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
  (is (= 1723 (p/p8-a))))
