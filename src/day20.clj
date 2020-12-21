(ns day20
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))

(def input
  (slurp "./input/day20.txt"))

(def test-input "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
")

(defn parse-tile
  [[header & lines]]
  (let [id (Long/parseLong (.substring header 5 9))]
    {:tile-id id
     :rotation 0     ;; = * pi/2 CCW
     :flipped false
     :lines lines}))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (partition-by empty?)
       (filter #(< 2 (count %)))
       (map parse-tile)))

(defn pixel-set?
  "returns true if pixel set at position

  (0,0) is in the top left corner"
  [lines x y]
  (= \# (nth (nth lines y) x)))

(test/with-test
  (defn side-ids
    [lines]
    (reduce
     (fn [sides i]
       (let [ii (- 10 i 1)]
         (-> sides
             (update 0 #(bit-or %
                                (if (pixel-set? lines ii 0)
                                  (bit-shift-left 1 i)
                                  0)))
             (update 1 #(bit-or %
                                (if (pixel-set? lines 9 ii)
                                  (bit-shift-left 1 i)
                                  0)))
             (update 2 #(bit-or %
                                (if (pixel-set? lines i 9)
                                  (bit-shift-left 1 i)
                                  0)))
             (update 3 #(bit-or %
                                (if (pixel-set? lines 0 i)
                                  (bit-shift-left 1 i)
                                  0))))))
     [0 0 0 0]
     (range 10)))

  (test/is (= [210 89 924 318] (side-ids (:lines (first (parse-input test-input)))))))

(test/with-test
  (def reverse-side-id
    (memoize
     (fn [x]
       (reduce
        #(bit-or
          (bit-shift-left %1 1)
          (if (bit-test x %2) 1 0))
        0
        (range 10)))))
  (test/is (= 512 (reverse-side-id 1)))
  (test/is (= 1 (reverse-side-id 512)))
  (test/is (every? true? (map #(= % (reverse-side-id (reverse-side-id %))) (range 1024)))))

(defn rotated-side-ids
  [top left bottom right]
  {0 [top left bottom right]
   1 [left bottom right top]
   2 [bottom right top left]
   3 [right top left bottom]})

(defn add-side-ids
  [tile-data]
  (->> tile-data
       (map
        #(let [sids (side-ids (:lines %))]
           (assoc % :side-ids (vec sids))))))

(defn side-id-map
  [tile-data]
  (->> tile-data
       (mapcat #(mapv vector (:side-ids %) (repeat (:tile-id %))))
       (reduce
        (fn [m [side-id tile-id]]
          (-> m
              (update side-id #(conj (or % #{}) tile-id))
              (update (reverse-side-id side-id) #(conj (or % #{}) tile-id))))
        {})))

(defn add-possible-neighbours
  [sim-map tile]
  (let [side-ids (:side-ids tile)
        tile-id (:tile-id tile)]
    (assoc tile
           :side-matches
           (mapv #(disj
                   (set/union
                    (sim-map %)
                    (sim-map (reverse-side-id %)))
                   tile-id)
                 side-ids))))

(defn enrich-with-side-matches
  [tiles]
  (let [sim (side-id-map tiles)]
    (map #(add-possible-neighbours sim %) tiles)))

(defn tile-map
  [tile-data]
  (->> tile-data
       (map #(hash-map (:tile-id %) %))
       (into {})))

(defn prepare-data
  [input]
  (->> input
       parse-input
       add-side-ids
       enrich-with-side-matches
       tile-map))

(defn find-corner-tiles
  [tile-map]
  (->> (vals tile-map)
       (filter #(= 2 (count (apply set/union (:side-matches %)))))))

(defn rotate-vec
  [v]
  (conj (subvec v 1) (v 0)))

(defn flip-tile
  [tile]
  (-> tile
      (update :flipped not)
      (update :side-ids #(vector (reverse-side-id (% 0))
                                 (reverse-side-id (% 3))
                                 (reverse-side-id (% 2))
                                 (reverse-side-id (% 1))))
      (update :side-matches #(vector (% 0) (% 3) (% 2) (% 1)))
      (update :lines #(map (comp str/join reverse) %))))

(defn rotate-lines
  [lines]
  (for [i (range 10)]
    (str/join (map #(nth % (- 9 i)) lines))))

(defn rotate-tile
  [tile]
  (-> tile
      (update :rotation #(mod (inc %) 4))
      (update :side-ids rotate-vec)
      (update :side-matches rotate-vec)
      (update :lines rotate-lines)))

(defn flip-tile-if-needed
  [side-id tile]
  (if (contains? (set (:side-ids tile)) (reverse-side-id side-id))
    tile
    (flip-tile tile)))

(defn rotate-till-side-aligns
  [side-id side tile]
  (->> tile
       (iterate rotate-tile)
       (take 4)
       (drop-while #(not= (nth (:side-ids %) side) (reverse-side-id side-id)))
       first))

(defn align-side
  [side-id side tile]
  (->> tile
       (flip-tile-if-needed side-id)
       (rotate-till-side-aligns side-id side)))

(defn find-topleft-tile
  [tile-map]
  (->> tile-map
       find-corner-tiles
       first
       (iterate rotate-tile)
       (take 4)
       (drop-while #(let [sides (:side-matches %)]
                       (or (empty? (nth sides 1))
                           (empty? (nth sides 2)))))
       first))

(defn tile-to-side
  [tiles side tile]
  (let [pos-tiles (nth (:side-matches tile) side)
        side-id (nth (:side-ids tile) side)
        other-side (mod (+ 2 side) 4)]
    (->> pos-tiles
         (map tiles)
         (map #(align-side side-id other-side %))
         first)))

(defn tiles-to-side
  [tiles side tile]
  (->> tile
       (iterate #(tile-to-side tiles side %))
       (take-while #(not (empty? %)))))

(defn link-neighbors
  "link to neighbours to right and down"
  [tiles]
  (let [topleft (find-topleft-tile tiles)
        left-col (tiles-to-side tiles 2 topleft)]
    (for [cell left-col]
      (tiles-to-side tiles 1 cell))))

(defn extract-image-from-linked-neighbors
  [linked-neighbors]
  (->>
    (for [row linked-neighbors]
        (->> row
            (map :lines)
            (apply map vector)
            (drop 1)
            (take 8)
            (map (fn [row] (str/join (map #(.substring % 1 9) row))))))
    flatten))


(def nessie (->> "                  # 
#    ##    ##    ###
 #  #  #  #  #  #   "
                 str/split-lines))

(defn print-image
  [image]
  (doseq [line image]
    (println line)))

(defn rotate-image
  [image]
  (->> image
       (apply mapv vector)
       (map str/join)
       reverse))

(defn flip-image
  [image]
  (->> image
       (map reverse)
       (map str/join)))

(defn find-monsters-in-image
  [monster image]
  (let [h_monster (count monster)
        w_monster (count (first monster))
        h_image   (count image)
        w_image   (count (first image))
        img       (vec image)
        patterns  (map #(re-pattern (str/replace % " " ".")) monster)]
    (for [row (range (- h_image (dec h_monster)))
          :let [lines (take h_monster (drop row img))]
          col (range (- h_image (dec w_monster)))
          :let [windows (map #(.substring % col (+ col w_monster)) lines)]
          :when (every? identity (mapv #(re-find %1 %2) patterns windows))]
     [row, col]))) 
      
(defn find-monsters
  [monster image]
  (->> (concat
        (take 4 (iterate rotate-image image))
        (take 4 (iterate rotate-image (flip-image image))))
       (map #(vector % (find-monsters-in-image monster %)))
       (remove #(empty? (second %)))
       first))

(defn mark-monster
  [img [y x]]
  
  (update (vec img) y
          #(str (subs % 0 x)
                "O"
                (subs % (inc x)))))

(defn remove-monster
  [monster image [x y]]
  (->>
    (for [r (range (count monster))
            :let [line (nth monster r)]
            c (range (count (first monster)))
            :when (= \# (nth line c))]
      [(+ x r) (+ y c)])
    (reduce mark-monster image)))

  

(defn remove-monsters
  [monster [image locations]]
  (reduce (partial remove-monster monster) image locations)) 

(test/with-test
  (defn part1 [input]
    (->> input
         prepare-data
         find-corner-tiles
         (map :tile-id)
         (reduce *')))

  (test/is (=  20899048083289  (part1 test-input))))

(defn count-pixels
  [img]
  (count
    (for [row img
            c  row
            :when (= c \#)]
      1)))
  
(test/with-test
  (defn part2 [input]
    (->> input
         prepare-data
         link-neighbors
         extract-image-from-linked-neighbors
         (find-monsters nessie)
         (remove-monsters nessie)
         count-pixels))

  (test/is (= 273 (part2 test-input))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))

  (crit/bench (part2 input))

  (def test-data (prepare-data test-input))
  (def test-tile (first (vals test-data)))
  (def test-tile-lines (:lines test-tile))
  (def part1-data (prepare-data input))

  (def top-left-tile (find-topleft-tile test-data)))

