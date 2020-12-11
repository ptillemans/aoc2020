(ns day10
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day10.txt"))

(def test-input
  "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(defn parse-input
  [input]
  (->> input str/split-lines (map #(Long/parseLong %))))


(defn diffs-between
  [numbers]
  (->> (map vector (rest numbers) numbers)
       (map #(- (first %) (second %)))))

(defn add-implicit-adapters
  [numbers]
  (cons 0 (cons (+ (apply max numbers) 3) numbers)))

(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         add-implicit-adapters
         sort
         diffs-between
         (group-by identity)
         (map #(count (second %)))
         (reduce *)))

  (test/is (= 220 (part1 test-input))))

(defn calc-combinations
  [m [n & numbers]]
  (let [counts (filter identity (map m (range (- n 3) n)))
        sum (reduce + counts)]
    (do
      (if (empty? numbers)
        sum
        (recur (assoc m n sum) numbers)))))

(test/with-test
  (defn part2 [input]
    (->> input
         parse-input
         add-implicit-adapters
         sort
         rest
         (calc-combinations {0 1})))

  (test/is (= 19208 (part2 test-input))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

