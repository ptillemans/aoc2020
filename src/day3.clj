(ns day3
  (:require [clojure.string :as str]
            [clojure.test :as test]
            [criterium.core :as crit]))

(def test-input
  [ "..##.......",
    "#...#...#..", 
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"])
  
(def input
  (-> "input/day3.txt"
      slurp
      str/split-lines))

(test/with-test
  (defn parse-line
    ([line] (parse-line line 0 #{}))    
    ([line start-pos positions]
     (let [p (str/index-of line \# start-pos)]
       (if (not p)
         [(count line) positions]
         (recur line (inc p) (conj positions p))))))

  (test/is (= [11 #{2 3}] (parse-line (first test-input))))
  (test/is (= [11 #{0 4 8}] (parse-line (second test-input)))))

(test/with-test
  (defn count-trees
    "count number of trees in "
    ([forrest ratio] (count-trees forrest ratio 0 0))
    ([forrest [dy dx] position count]
     (if (empty? forrest)
       count
       (let [[width trees] (parse-line (first forrest))
             tree? (contains? trees position)
             new_count (if tree? (inc count) count)
             new_position (mod (+ position dx) width)
             new_forrest (drop dy forrest)]
         (recur new_forrest [dy dx] new_position new_count)))))

      
      
  (test/is 7 (count-trees test-input [1 3] 0 0)))


(test/with-test
  (defn part1[data]
    (count-trees data [1 3]))
  
  (test/is 7 (part1 test-input)))

(def slopes
  [[1 1],
   [1 3],
   [1 5],
   [1 7],
   [2 1]])

(test/with-test
  (defn part2 [data]
    (->> slopes
         (map #(count-trees data %))
         (reduce *)))
  
  (test/is 336 (part2 test-input)))
        
             
(comment
  (part1 input)
  (part2 input)
  (test/run-tests))
