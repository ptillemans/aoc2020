(ns day24
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))

(def input
  (slurp "./input/day24.txt"))

(def test-input "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(defn tokenizer
  [tokens char]
  (condp = (first tokens)
    :n (cons (keyword (str "n" char)) (rest tokens))
    :s (cons (keyword (str "s" char)) (rest tokens))
    (cons (keyword char) tokens)))

(defn parse-line [line]
  (->> (str/split line #"")
       (reduce tokenizer '())
       reverse))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map parse-line)))

;; use a coordinate system
;; N  [0 1]
;; NE [1 1]
;; E  [2 0]
;; SE [1 -1]
;; SW [-1 -1]
;; W  [-2 0]
;; NW [-1 1]

(def dir-to-delta
  {:ne [1 1]
   :e  [2 0]
   :se [1 -1]
   :sw [-1 -1]
   :w  [-2 0]
   :nw [-1 1]})

(test/with-test
  (defn delta-path
    [path]
    (reduce
     #(mapv + %1 (dir-to-delta %2))
     [0 0]
     path))

  (test/is (= [0 0] (delta-path (parse-line "nwwswee")))))

(defn flip-tile
  "incremetn number of flips for  a tile"
  [floor location]
  (update floor location #(inc (or % 0))))

(defn flip-tiles
  [floor paths]
  (->> paths
       (map delta-path)
       (reduce flip-tile floor)))

(defn count-black-tiles
  [floor]
  (->> floor
       vals
       (filter odd?)
       count))

(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         (flip-tiles {})
         count-black-tiles))

  (test/is (= 10 (part1 test-input))))

(defn black?
  [floor loc]
  (odd? (or (floor loc) 0)))

(defn count-black-neighbors
  [floor location]
  (->> (vals dir-to-delta)
       (map #(mapv + location %))
       (filter #(black? floor %))
       count))

(defn floor-bounds
  [floor]
  (->> floor
       keys
       (filter #(black? floor %))
       (apply mapv vector)
       (map #(vector (apply min %) (apply max %)))))

(defn all-tile-locations
  [floor]
  (let [[[xmin xmax] [ymin ymax]] (floor-bounds floor)
        dec2 (comp dec dec)
        inc2 (comp inc inc)
        inc3 #(+ % 3)]
    (for [x (range (dec2 xmin) (inc3 xmax))
          y (range (dec2 ymin) (inc2 ymax))
          :when (= (odd? x) (odd? y))]
      [x y])))

(defn next-tile
  [floor loc]
  (let [n-black   (count-black-neighbors floor loc)
        black-tile? (black? floor loc)
        white 0
        black 1]
    (cond
      (and black-tile? (or (= n-black 0) (> n-black 2))) {loc white}
      (and (not black-tile?) (= n-black 2)) {loc black}
      :otherwise {loc (if black-tile? black white)}))) 
      
    
(defn floor-next-day
  [floor]
  (->> floor
       all-tile-locations
       (map #(next-tile floor %))
       (into {})))
       
(defn floor-plan
  [nr-days floor]
  (->> (iterate floor-next-day floor)
       (map-indexed #(vector %1 (count-black-tiles %2)))
       (take (inc nr-days)))) 
  
(test/with-test
  (defn part2 [input]
    (->> input
         parse-input
         (flip-tiles {})
         (floor-plan 100)
         last
         second))

  (test/is (= 2208 (part2 test-input))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

