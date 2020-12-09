(ns day6
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
   (slurp "./input/day6.txt"))

(def test-input
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(test/with-test
  (defn part1 [input]
    (->> input
         split-groups
         (map count-answers-in-group)
         (reduce +)))

  (test/is (= 11 (part1 test-input))))

(defn split-groups
  [input]
  (str/split input #"\n\n"))

(defn count-answers-in-group
  [group]
  (->> (str/split-lines group)
       (mapcat #(str/split % #""))
       set
       count))
       
(test/with-test
  (defn part2 [input]
    (->> input
         split-groups
         (map common-answers-in-group)
         (reduce +)))

  (test/is (= 6 (part2 test-input))))

(defn common-answers-in-group
  [group]
  (->> group
       str/split-lines
       (map #(set (str/split % #"")))
       (reduce set/intersection)
       count))

(comment
  (test/run-tests)
  (split-groups test-input)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

