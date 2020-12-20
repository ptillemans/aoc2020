(ns day19
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]
            [clojure.pprint :as pp]))


(def input
  (slurp "./input/day19.txt"))

(def test-input
  "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb")

(defn stoi
  [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException e (println "ERROR: " s " is not a number"))))

(defn number-sequence
  [s]
  (do
    ;;(println "number-sequence: " s)
    (->> (str/split s #" ")
        (map stoi)
        (cons :seq)
        vec)))

(defn parse-rule
  [s]
  (let [[sid rl] (str/split s #": ")
        id       (stoi sid)]
    (do
      ;;(println "parse-rule: " s)
      (hash-map id
        (condp re-seq rl
          #"^(.*) \| (.*)$"
          :>> (fn [[[_all a b]]] 
                (vector :or
                        (number-sequence a)
                        (number-sequence b)))

          #"^\d+( \d+)*$"
          :>> #(number-sequence (first (first %)))

          #"\"(\w)\"" 
          :>> #(vector :lit (second (first %)))

          (do
            (println "*ERROR* unrecognized pattern >" rl "<.")
            (throw (.Exception "Unrecognized pattern")))))))) 

(defn parse-rules
  [lines]
  (->> lines
       (map parse-rule)
       (into {})))


(defn create-node
  [m node]
  (vec
    (map
      #(cond
          (keyword? %) %
          (string? %) %
          (integer? %) (if (not= node (m %))
                         (create-node m (m %))
                         :loop)
          (coll? %) (create-node m %))
      node)))

(test/with-test
  (defn rule-map-to-ast
    [m p]
    (create-node m (m p)))

  (test/is (= [:lit "a"] (rule-map-to-ast { 0 [:lit "a"]} 0)))
  (test/is (= [:seq [:lit "a"] [:lit "b"]]
              (rule-map-to-ast { 0 [:seq 1 2] 1 [:lit "a"] 2 [:lit "b"]} 0)))
  (test/is (= [:or [:lit "a"] [:lit "b"]]
              (rule-map-to-ast { 0 [:or 1 2] 1 [:lit "a"] 2 [:lit "b"]} 0)))
  (test/is (= [:seq [:or [:lit "a"] [:lit "b"]] [:seq [:lit "b"] [:lit "a"]] [:lit "b"]]
              (rule-map-to-ast {0 [:seq 1 2 3]
                                1 [:or 4 3]
                                2 [:seq 3 4]
                                3 [:lit "b"]
                                4 [:lit "a"]} 0)))
  (test/is (= [:seq [:lit "a"] :loop]
              (rule-map-to-ast {0 [:seq 1 0]
                                1 [:lit "a"]} 0))))
  
(defn parse-input
  [s]
  (->> s
       str/split-lines
       (partition-by empty?)
       (#(let [rule-map (parse-rules (first %))]
           (hash-map
            :rule-map rule-map
            :rules (rule-map-to-ast rule-map 0)
            :messages (last %))))))

(test/with-test
  (defn match-message
    "returns unmatched part of message if matched or nil if not matched"
    [ast message]
    (and message
      (condp = (first ast)
        :lit (do
                ;;(println "match literal" (second ast) " to " message)
                (if (.startsWith message (second ast))
                  (.substring message 1)))
        :or (some #(match-message % message) (rest ast))
        :seq (do
               (println "seq: " message ast)
               (reduce #(if (not= %2 :loop)
                          (match-message %2 %1)
                          (do
                            (println "loop" ast %1)
                            (match-message ast %1)))
                     message (rest ast)))
        (println "No match for " message " using " ast))))

  (test/is (= "" (match-message [:lit "a"] "a")))
  (test/is (= "a" (match-message [:lit "a"] "aa")))
  (test/is (= nil (match-message [:lit "a"] "")))
  (test/is (= "" (match-message [:seq [:lit "a"] [:lit "b"]] "ab")))
  (test/is (= nil (match-message [:seq [:lit "a"] [:lit "b"]] "a")))
  (test/is (= nil (match-message [:seq [:lit "a"] [:lit "b"]] "b")))
  (test/is (= nil (match-message [:seq [:lit "a"] [:lit "b"]] "ba")))
  (test/is (= "a" (match-message [:seq [:lit "a"] [:lit "b"]] "aba")))
  (test/is (= "" (match-message [:or [:lit "a"] [:lit "b"]] "a")))
  (test/is (= "" (match-message [:or [:lit "a"] [:lit "b"]] "b"))))


(test/with-test
  (defn valid-message?
    [ast message]
    (= "" (match-message ast message)))

  (test/is
    (let [{:keys [rules messages]} (parse-input test-input)]
      (map #(valid-message? rules %) messages))))

(test/with-test
  (defn part1 [input]
    (let [{:keys [rules messages]} (parse-input input)]
      (->> messages
           (filter #(valid-message? rules %))
           count)))        
  (test/is (= 2 (part1 test-input))))

(def test-input2 "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")


(defn patch-rules
  [rule-map]
  (-> rule-map
    (assoc 8 [:or [:seq 42] [:seq 42 8]])
    (assoc 11 [:or [:seq 42 31] [:seq 42 11 31]])))

(declare match-messages)

(defn spy
  ([x] (spy "spy" x))
  ([s x]
   (do
     (println s ": <" x ">")
     x)))

(defn apply-seq
  [rules messages args]
  (do
;;    (println "apply-seq" messages args)
    (->> (for [msg  messages]
          (reduce (fn [msgs rule]
                    (mapcat #(match-messages rules rule %) msgs))
                  [msg] args))
         flatten
         vec)))
;;         (spy (str "seq" args)))))

(defn apply-or
  [rules messages args]
  (do
;;    (println "apply-or " messages args)
    (->> (mapcat #(apply-seq rules messages %1) (map rest args))
         vec)))
;;         (spy (str "or" args)))))

(test/with-test
  (defn match-messages
    ([rules message]
     (match-messages rules 0 message))
    ([rules p message]
     (do
      ;;(println "match-messages" p message " | " (rules p))
      (and
       message
       (let [rule (rules p)
             [type & args] rule
             rslt (condp = type
                    :lit (if (.startsWith message (first args))
                            (vector (.substring message 1)))
                    :seq (apply-seq rules [message] args)
                    :or (apply-or rules [message] args))]
         (do
;;           (if (not (= :lit (first (rules p))))
;;            (println "match-messages" p message "-->" (str rslt) (rules p))
           rslt))))))
    
  (test/is (= [""] (match-messages {0 [:lit "a"]} "a")))
  (test/is (= ["a"] (match-messages {0 [:lit "a"]} "aa")))
  (test/is (= nil (match-messages {0 [:lit "a"]} "")))
  (test/is (= [""] (match-messages  {0 [:seq 1 2]
                                     1 [:lit "a"]
                                     2 [:lit "b"]} "ab")))
  (test/is (= [] (match-messages  {0 [:seq 1 2]
                                   1 [:lit "a"]
                                   2 [:lit "b"]} "a")))
  (test/is (= [] (match-messages  {0 [:seq 1 2]
                                   1 [:lit "a"]
                                   2 [:lit "b"]} "b")))
  (test/is (= [] (match-messages  {0 [:seq 1 2]
                                   1 [:lit "a"]
                                   2 [:lit "b"]} "ba")))
  (test/is (= ["a"] (match-messages  {0 [:seq 1 2]
                                      1 [:lit "a"]
                                      2 [:lit "b"]} "aba")))
  (test/is (= [""] (match-messages {0 [:or [:seq 1] [:seq 2]]
                                    1 [:lit "a"]
                                    2 [:lit "b"]} "a")))
  (test/is (= [""] (match-messages {0 [:or [:seq 1] [:seq 2]]
                                    1 [:lit "a"]
                                    2 [:lit "b"]} "b")))
  (test/is (= [] (match-messages {0 [:or [:seq 1] [:seq 2]]
                                    1 [:lit "a"]
                                    2 [:lit "b"]} "")))
  (test/is (= [""] (match-messages {0 [:seq 3 1]
                                    1 [:or [:seq 3] [:seq 3 1]]
                                    2 [:seq 3 0]
                                    3 [:lit "a"]
                                    4 [:lit "b"]} "aa")))
  (test/is (= ["a" ""]
              (match-messages {0 [:seq 3 1]
                               1 [:or [:seq 3] [:seq 3 1]]
                               2 [:seq 3 0]
                               3 [:lit "a"]
                               4 [:lit "b"]}
                              "aaa")))
  (test/is (= [""])
           (let [{:keys [rule-map messages]} (parse-input test-input2)
                 alt-map (patch-rules rule-map)]
                (match-messages alt-map (second messages))))
  (test/is (= [""])
           (let [{:keys [rule-map messages]} (parse-input test-input2)
                 alt-map (patch-rules rule-map)]
                (match-messages alt-map 11 "bbaabaabba"))))
  

(defn valid-message2?
  [rules message]
  (contains?
   (->> message
       (match-messages rules)
       set)
   ""))

(test/with-test
  (defn part2 [input]
    (let [{:keys [rule-map messages]} (parse-input input)
          alt-map (patch-rules rule-map)]
      (->> messages
           (filter #(valid-message2? alt-map %))
           count)))  

  (test/is (= 12 (part2 test-input2))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

