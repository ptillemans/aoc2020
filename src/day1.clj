(ns day1
  (:require [clojure.string :as str]
            [clojure.test :as test]))


(def data (->> (slurp "input/day1.txt")
               (str/split-lines)
               (map #(Integer/parseInt %))))

(def test-data [1721 979 366 299 675 1456])

(defn find-pair-sorted-lists
  [forward backward total]
  (if (or (empty? forward)
          (empty? backward)
          (> (first forward) (first backward)))
    ()
    (let [sum-first (+ (first forward) (first backward))]
      (cond
        (> sum-first total) (find-pair-sorted-lists forward (rest backward) total)
        (< sum-first total) (find-pair-sorted-lists (rest forward) backward total)
        :else [(first forward) (first backward)]))))

(test/with-test
  (defn find-pair-with-sum
    [data total]
    (find-pair-sorted-lists (sort data) (reverse (sort data)) total))

  (test/is (find-pair-with-sum test-data 2020) [299 1721]))

(defn part1
  []
  (let [[x y] (find-pair-with-sum data 2020)]
    (* x y)))

(test/with-test
  (defn find-triplet-with-sum
    [data total]
    (if  (empty? data)
        ()
        (let [[h & r] data
              pair (find-pair-with-sum r (- total h))]
            (if pair
                (concat [h] pair)
                (recur r total)))))
  (test/is (find-triplet-with-sum (sort data) 2020) [366 679 979]))

(defn part2 []
  (apply * (find-triplet-with-sum (sort data) 2020)))

(comment
  (part1)
  (part2)
  (test/run-tests))
