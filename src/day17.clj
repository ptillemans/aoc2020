(ns day17
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))


(def input
  (slurp "./input/day17.txt"))

(def test-input ".#.
..#
###")

(test/with-test
  (defn parse-input
    [input]
    (let [lines (str/split-lines input)
          size  (count lines) 
          half-size (quot size 2)
          dim    (range (- size) (inc size))]
      (into #{}    
        (for [y (range size) 
              x (range size)
              :when (= \# (nth (nth lines y) x))]
            [(- x half-size) (- y half-size) 0])))) 

  (test/is (= #{[0 -1 0] [1 0 0] [-1 1 0] [0 1 0] [1 1 0]} (parse-input test-input))))


(defn count-neighbours
  [state p]
  (->> p
       (map #(range (- % 1) (+ % 2)))
       (apply combo/cartesian-product)
       (filter #(not= % p))
       (filter #(contains? state %))
       count))
  
(defn bounds
  [state]
  (let [mins (reduce #(map min %1 %2) state)
        maxs (reduce #(map max %1 %2) state)]
    (map #(vector %1 %2) mins maxs)))

(defn becomes-active?
  [state p]
  (let [nn (count-neighbours state p)]
    (if (contains? state p)
      (<= 2 nn 3)
      (= nn 3))))

(defn evolve
  [state]
  (->> state
       bounds
       (map #(range (dec (first %)) (+ (second %) 2)))
       (apply combo/cartesian-product)
       (filter #(becomes-active? state %))
       (into #{})))

(defn print-state
  [state]
  (let [[bx by bz] (bounds state)]
    (println bx by bz)
    (doall
      (for [z (range (first bz) (inc (second bz)))]
        (do
          (println)
          (println "z=" z)
          (doall
            (for [y (range (first by) (inc (second by)))]
              (->> (range (first bx) (inc (second bx)))
                  (map #(if (contains? state (vector % y z)) \# \.))
                  (str/join)
                  (println (format "%3d: " y))))))))))
              
         
(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         (iterate evolve)
         (drop 6)
         first
         count))

  (test/is (= 112 (part1 test-input))))


(test/with-test
  (defn part2 [input]
    (->> input
         parse-input
         (map #(conj % 0))
         (into #{})
         (iterate evolve)
         (drop 6)
         first
         count))

  (test/is (= 848 (part2 test-input))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

