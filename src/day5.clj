(ns day5
  (:require [clojure.string :as str]
            [clojure.test :as test]
            [criterium.core :as crit]))

(def test-input 
   [{:code "BFFFBBFRRR", :row 70, :column 7, :seat 567},
    {:code "FFFBBBFRRR", :row 14, :column 7, :seat 119},
    {:code "BBFFBBFRLL", :row 102, :column 4, :seat 820}])

(def input
  (-> (slurp "./input/day5.txt")
      str/split-lines))

(test/with-test
  (defn parse-ticket
    [code]
    (let [accumulate (fn [s c] (+ (* s 2) (if (contains? #{\B \R} c) 1 0)))
          seat (reduce accumulate 0 code)
          col (mod seat 8)
          row (quot seat 8)]
      {:code code, :seat seat, :row row, :column col})) 
    
  (test/is (every? #(= % (parse-ticket (:code %))) test-input)))


(defn part1 [input]
  (->> input
      (map parse-ticket)
      (map #(:seat %))
      (apply max)))

(defn part2 [input]
  (let [seats (sort (map #(:seat (parse-ticket %)) input))
        zip-seats (map vector (rest seats) seats)
        seats-after (drop-while #(not= 2 (apply - %)) zip-seats)
        next-seat (first (first seats-after))]
    (- next-seat 1)))
    
(comment
  (parse-ticket "BFFFBBFRRR")
  (parse-ticket "FFFBBBFRRR")
  (parse-ticket "BBFFBBFRLL")
  (test/run-tests)
  (part1 (map :code test-input))
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))
