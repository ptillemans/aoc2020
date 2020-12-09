(ns day7
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
    (slurp "./input/day7.txt"))

(def test-input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defn parse-bag
  [bag-string]
  (str/replace bag-string #" bag.*" ""))

(defn parse-contents
  [bags]
  (if (str/starts-with? (first bags) "no ")
    []
    (->> bags
         (map #(str/split % #" " 2))
         (map (fn [[a b]]
                {:amount (Integer/parseInt a)
                 :bag (parse-bag b)})))))

(defn parse-line
  [line]
  (let [bags (str/split line #" contain |, ")
        bag (parse-bag (first bags))
        contents (parse-contents (rest bags))]
    [bag {:contents contents :containers []}]))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (mapcat parse-line)
       (apply hash-map)))

(defn add-container-to-bag
  [container bag bags]
  (update-in bags [bag :containers]
             #(conj % container)))

(defn add-container-to-contents
  [bags container]
  (->> (get-in bags [container :contents])
       (map :bag)
       (reduce
        (fn [m b] (add-container-to-bag container b m))
        bags)))

(defn add-container-to-bags
  [bags]
  (->> (keys bags)
    (reduce #(add-container-to-contents %1 %2) bags)))

(defn possible-containers
  [bag bags]
  (let [containers (get-in bags [bag :containers])]
    (if (empty? containers)
      #{}
      (set (mapcat #(cons % (possible-containers bags %)) containers)))))


(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         add-container-to-bags
         possible-containers "shiny gold"))

  (test/is (= 4 (part1 test-input))))

(test/with-test
(defn part2 [input]
  (->> input))

(test/is (= test-input (part2 test-input))))

(comment
(test/run-tests)
(part1 input)
(part2 input)
(crit/bench (part1 input))
(crit/bench (part2 input)))

