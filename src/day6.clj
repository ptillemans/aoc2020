(ns day6
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]))


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
    ())

  (test/is (= 11 (part1 test-input))))

(defn split-groups
  [input]
  (str/split input #"\n\n"))

(defn count-answers-in-group
  [group]
  (->> (str/split-lines group)
       (mapcat #(str/split % #""))))
       

(comment
  (test/run-tests)
  (split-groups test-input)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

