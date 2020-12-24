(ns day23
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))

(def input
  "318946572")

(defn parse-input [input]
  (->> input
       (#(str/split % #""))
       (mapv #(Integer/parseInt %))))

(defn cup-dec [n & {:keys [exclude]
                    :or {exclude #{}}}]
  (let [result (inc (mod (- n 2) 9))]
    (if (contains? exclude result)
      (cup-dec result :exclude exclude)
      result)))

(defn rotate-right
  [xs]
  (let [[r [l]] (split-at (dec (count xs)) xs)]
    (vec (cons l r))))

(defn next-array
  [cups]
  (->> cups
       (map vector (rotate-right cups))
       sort
       (map second)
       (map int)
       (map dec)
       (into-array)))


(defn round
  [na current]
  (let [t1  (aget na current)
        t2  (aget na t1)
        t3  (aget na t2)
        destination (cup-dec current :exclude #{t1 t2 t3})]
    (aset na current (aget na t3))
    (aset na t3 (aget na destination))
    (aset na destination t1)
    (aget na current)))

(defn into-cups
  [na current]
  (->>
   (iterate #(aget na %) current)
   (map inc)
   (take (alength na))))

(defn game
  [n cups]
  (let [na (next-array cups)
        start (first cups)]
    (->> start
         (iterate #(round na %))
         (drop n)
         first)
    (into-cups na 1)))
         

(defn format-cups
  [cups]
  (->> cups
       cycle
       (drop-while #(not= 1 %))
       (drop 1)
       (take 8)
       (map str)
       str/join))

(defn part1 [input]
  (->> input
       parse-input
       (game 100)
       format-cups))

(defn cups2
  [cups]
  (let [m (apply max cups)]
    (->> (iterate inc (inc m))
         (concat cups)
         (take 1000000))))

(defn part2 [input]
  (let [results (->> input
                  parse-input
                  cups2
                  (game 10000000)
                  (take 3))]
    (println "Results:" results)
    (println "Part2:" (reduce * results))))
       

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

