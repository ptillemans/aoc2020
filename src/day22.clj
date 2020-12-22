(ns day22
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))

(def input
  (slurp "./input/day22.txt"))

(def test-input "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(defn parse-deck
  [lines]
  (->> lines
       (drop 1)
       (mapv #(Integer/parseInt %))))

(test/with-test
  (defn parse-input
    [input]
    (->> input
         str/split-lines
         (partition-by empty?)
         (filter #(> (count %) 1))
         (mapv parse-deck)))

  (test/is (= [[9 2 6 3 1] [5 8 4 7 10]]
              (parse-input test-input))))

(test/with-test
  (defn play-round
    [decks]
    (let [table (mapv first decks)
          best-card (apply max table)]
      (mapv #(if (= %1 best-card)
               (vec (concat (rest %2) (reverse (sort table))))
               (vec (rest %2)))
            table decks)))

  (test/is (= [[2 6 3 1 9 5] [8 4 7 10]]
              (play-round (parse-input test-input))))
  (test/is (= [[2] [4 3 1]]
              (play-round [[1 2] [3 4]]))))

(defn take-until
  ([pred coll acc]
   (when-let [[x & xs] coll]
     (if (pred x)
       (reverse (cons x acc))
       (recur pred xs (cons x acc)))))
  ([pred coll]
   (take-until pred coll '())))

(test/with-test
  (defn play-game
    [decks]
    (->> (iterate play-round decks)
         (take-until #(some empty? %))))

  (test/is (= [[] [3 2 10 6 8 5 9 4 7 1]])
           (last (play-game (parse-input test-input)))))

(test/with-test
  (defn score-deck
    [deck]
    (reduce +
            (mapv #(* %1 %2) (reverse deck) (iterate inc 1))))

  (test/is (= 10 (score-deck [1 2 3])))
  (test/is (= 0 (score-deck []))))

(test/with-test
  (defn score-game
    [decks]
    (mapv score-deck decks))
  (test/is (= [0 10]
              (score-game [[] [1 2 3]]))))

(test/with-test
  (defn part1 [input]
    (->> input
        parse-input
        play-game
        last
        score-game
        (apply max)))

  (test/is (= 306 (part1 test-input))))

(declare play-recursive-game)

(test/with-test
  (defn find-winner
    [table ndecks]
    (let [best-card (apply max table)
          ndeck-not-smaller-turned-card? (mapv #(>= (count %1) %2) ndecks table)
          need-sub-game? (every? true? ndeck-not-smaller-turned-card?)]
      (if need-sub-game?
        (->>
          (play-recursive-game (mapv #(take %1 %2) table ndecks))
          (mapv #(not (empty? %))))
        (mapv #(= %1 best-card) table))))

  (test/is (= [true false] (find-winner [9 5] [[2 6 3 1] [8 4 7 10]])))
  (test/is (= [false true] (find-winner [3 5] [[] [4]]))))


(test/with-test
  (defn new-decks
    [table ndecks]
    (let [winners (find-winner table ndecks)
          winning (->>
                    (map #(vector %1 %2) winners table)
                    (sort-by first)
                    (map second)
                    reverse
                    vec)]
      (mapv #(if %1
              (vec (concat %2 winning))
              (vec %2))
            winners ndecks)))

  (test/is (= [[2 6 3 1 9 5] [8 4 7 10]] (new-decks [9 5] [[2 6 3 1] [8 4 7 10]])))
  (test/is (= [[] [4 5 3]] (new-decks [3 5] [[] [4]])))
  (test/is (= [[5 1 2] [3 4]]
              (new-decks [1 2] [[5] [3 4]]))))


(test/with-test
  (defn play-recursive-round
    [decks]
    (let [table (mapv first decks)
          ndecks (mapv #(vec (rest %1)) decks)]
      (new-decks table ndecks)))

  (test/is (= [[2 6 3 1 9 5] [8 4 7 10]]
              (play-recursive-round (parse-input test-input))))
  (test/is (= [[2] [4 3 1]]
              (play-recursive-round [[1 2] [3 4]])))
  (test/is (= [[] [3 2 1]] 
              (play-recursive-round [[1] [2 3]])))
  (test/is (= [[5 1 2] [3 4]]
              (play-recursive-round [[1 5] [2 3 4]]))))



(test/with-test
  (defn play-recursive-game
    ([decks decks-seen]
     (cond
       (some empty? decks) decks
       (contains? decks-seen decks) (do
                                      (println "break")
                                      [[1] []])
       :otherwise (recur (play-recursive-round decks) (conj decks-seen decks))))
    ([decks]
     (play-recursive-game decks #{})))

  (test/is (= [[3] []]) (last (play-recursive-game [[3] []])))
  (test/is (= [[1] []]) (last (play-recursive-game [[43 19] [2 29 14]])))
  (test/is (= [[] [7 5 6 2 4 1 10 8 9 3]]
              (play-recursive-game (parse-input test-input)))))


(test/with-test
  (defn part2 [input]
    (->> input
         parse-input
         play-recursive-game
         score-game
         (apply max)))

  (test/is (= 291 (part2 test-input))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))
