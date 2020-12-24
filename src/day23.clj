(ns day23
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]
            [clojure.pprint :as pp]))

(def input
  "318946572")

(def test-input
  "389125467")

(defn parse-input [input]
  (->> input
       (#(str/split % #""))
       (mapv #(Integer/parseInt %))))

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
       (map dec)
       (map int)
       int-array))


(defn round
  [na size current]
  ;;(println "Current : " current)
  (let [t1  (aget ^ints na current)
        t2  (aget ^ints na t1)
        t3  (aget ^ints na t2)
        destination (->>
                     (iterate #(if (zero? %) (dec size) (dec %)) current)
                     (filter #(and (not= % t1) (not= % t2) (not= % t3)))
                     second)]
    ;;(println "Pick : " t1 t2 t3)
    ;;(println "Destination : " destination)
    ;;(pp/pprint na)
    (aset ^ints na current (aget ^ints na t3))
    ;;(pp/pprint na)
    (aset ^ints na t3 (aget ^ints na destination))
    ;;(pp/pprint na)
    (aset ^ints na destination t1)
    ;;(pp/pprint na)
    ;;(println "return: " (aget ^ints na current))
    (aget ^ints na current)))

(defn into-cups
  [na current]
  (->>
   (iterate #(aget na %) (dec current))
   (map inc)
   (take (alength na))))

(defn game
  [n cups]
  (let [na (next-array cups)
        size (count na)
        start (dec (first cups))]
    (->> start
         (iterate #(round na size %))
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

(def test-cups (->> test-input
                    parse-input
                    cups2))
                    
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

