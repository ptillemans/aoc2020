(ns day11
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day11.txt"))

(def test-input
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defn lines-to-map
  [lines]
  (->>
    (for [i (range (count lines))
          :let [line (nth lines i)]
          j (range (count line))
          :let [char (nth line j)]
          :when (not= char '\.)]
        {[i j] (if (= char \#) true nil)})
    (into {})))

(defn occupied-neighbors
  [loc state]
  (for [i (range -1 2)
        j (range -1 2)
        :let [x (+ (first loc) i)
              y (+ (second loc) j)]
        :when (not (and (zero? i) (zero? j)))
        :when (true? (state [x  y]))]
    [x y]))

(defn count-occupied-neighbors
  [loc state]
  (count (occupied-neighbors loc state)))

(defn evolve-chair
  [chair state]
  (let [[loc occupied?] chair]
    { loc
      (condp > (count-occupied-neighbors loc state)
        1 true
        4 occupied? 
        nil)}))

(defn evolve-state
  [state]
  (->> state
       (map #(evolve-chair % state))
       (into {})))

(defn evolve-till-stable
  [state]
  (let [new-state (evolve-state state)]
    (if (= state new-state)
      state
      (recur new-state))))

(defn _bounds
  [state]
  [(inc (apply max (map #(first (first %)) state)))
   (inc (apply max (map #(second (first %)) state)))])

(def bounds (memoize _bounds))

(defn show-state
  [state]
  (let [[max-x max-y] (bounds state)]
    (str/join "\n"
      (for [i (range max-x)]
        (->> (for [j (range max-y)]
                (if (contains? state [i j])
                  (if (state [i j]) \# \L)
                  \.))
          (apply str))))))

(defn print-state
  [state]
  (println (show-state state)))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       lines-to-map))

(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         evolve-till-stable
        (map second)
        (filter true?)
        count))

  (test/is (=  37 (part1 test-input))))

(defn in-bounds?
  [bounds loc]
  (and (<= 0 (first loc) (dec (first bounds)))
       (<= 0 (second loc) (dec (second bounds)))))

(def test-state1 (parse-input ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#....."))

(def test-state2 (parse-input ".............
.L.L.#.#.#.#.
............."))

(def test-state3 (parse-input ".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##."))

(test/with-test
  (defn chair-occupied-in-dir
    [loc dir state]
    (let [new-loc (map  + loc dir)]
      (if (in-bounds? (bounds state) loc)
        (if (contains? state new-loc)
          (if (state new-loc) (vec new-loc) nil)  
          (recur new-loc dir state)))))
  (test/is
   (= [2 1]
      (chair-occupied-in-dir [4 3] [-1 -1] test-state1)))
  (test/is
   (= [1 3]
      (chair-occupied-in-dir [4 3] [-1 0] test-state1)))
  (test/is
   (= [0 7]
      (chair-occupied-in-dir [4 3] [-1 1] test-state1)))
  (test/is
   (= [4 2]
      (chair-occupied-in-dir [4 3] [0 -1] test-state1)))
  (test/is
   (= [4 8]
      (chair-occupied-in-dir [4 3] [0 1] test-state1)))
  (test/is
   (= [7 0]
      (chair-occupied-in-dir [4 3] [1 -1] test-state1)))
  (test/is
   (= [8 3]
      (chair-occupied-in-dir [4 3] [1 0] test-state1)))
  (test/is
   (= [5 4]
      (chair-occupied-in-dir [4 3] [1 1] test-state1)))
  (test/is
   (= nil (chair-occupied-in-dir [1 1] [0 1] test-state2))))

(def directions
  (for [x (range -1 2)
        y (range -1 2)
        :when (not (and (zero? x) (zero? y)))]
    [x y]))

(test/with-test
  (defn visible-occupied-chairs
    [loc state]
    (filter #(not (nil? %))
            (map #(chair-occupied-in-dir loc % state) directions)))
    
    

  (test/is (= 8 (count (visible-occupied-chairs [4 3] test-state1))))
  (test/is (= [] (visible-occupied-chairs [1 1] test-state2)))
  (test/is (= [] (visible-occupied-chairs [3 3] test-state3))))

(defn evolve-chair2
  [chair state]
  (let [[loc occupied?] chair]
    { loc
      (condp < (count (visible-occupied-chairs loc state))
        1 true
        5 occupied? 
        nil)}))

(defn evolve-state2
  [state]
  (->> state
       (map #(evolve-chair2 % state))
       (into {})))

(defn evolve-till-stable2
  [state]
  (let [new-state (evolve-state2 state)]
    (if (= state new-state)
      state
      (recur new-state))))


(test/with-test
  (defn part2 [input]
    (->> input
         parse-input
         evolve-till-stable2
         (map second)
         (filter identity)
         count)) 

  (test/is (= 26 (part2 test-input))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

