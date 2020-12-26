(ns day25
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day25.txt"))

(def test-input
  "5764801
17807724")

(test/with-test
  (defn parse-input [input]
    (->> input
        str/split-lines
        (mapv #(Integer/parseInt %))))

  (test/is (= [5764801 17807724] (parse-input test-input))))

(test/with-test
  (defn calculation-loop
    [subject]
    (iterate #(mod (* % subject) 20201227) 1))

  (test/is (= 5764801 (nth (calculation-loop 7) 8)))
  (test/is (= 17807724 (nth (calculation-loop 7) 11))))

(test/with-test
  (defn encryption-key
    [secret-loops public-key]
    (nth (calculation-loop public-key) secret-loops))

  (test/is (= 14897079 (encryption-key 11 5764801)))
  (test/is (= 14897079 (encryption-key 8 17807724))))

(test/with-test
  (defn find-secret-loop-size
    [public-key]
    (->> (calculation-loop 7)
         (map-indexed #(vector %1 %2))
         (drop-while #(not (= public-key (second %))))
         first))

  (test/is (= [8 5764801] (find-secret-loop-size 5764801)))
  (test/is (= [11 17807724] (find-secret-loop-size 17807724))))

(defn encryption-keys
  [[[sl1 pk1] [sl2 pk2]]]
  [(encryption-key sl1 pk2) (encryption-key sl2 pk1)])

(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         (mapv find-secret-loop-size)
         encryption-keys))
         

  (test/is (= [14897079 14897079] (part1 test-input))))

(test/with-test
  (defn part2 [input]
    (->> input))

  (test/is (= test-input (part2 test-input))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))
