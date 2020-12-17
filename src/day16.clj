(ns day16
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day16.txt"))

(def test-input
  "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(defn parse-range
  [range]
  (->> (str/split range #"-")
       (map #(Long/parseLong %))
       (apply vector)))

(defn parse-rule
  [line]
  (let [[field, criteria] (str/split line #": ")
        ranges (map parse-range (str/split criteria #" or "))]
    [field ranges]))

(defn parse-ticket
  [line]
  (->> (str/split line #",")
       (map #(Integer/parseInt %))))

(defn parse-input
  [input]
  (let [data (->> input
                str/split-lines
                (partition-by empty?)
                (filter #(> (count %) 1)))]
    {:rules (apply hash-map (mapcat parse-rule (first data)))
     :my-ticket (parse-ticket (second (second data)))
     :nearby-tickets (map parse-ticket (rest (last data)))}))

;;--------------------------------------------------------------------

(defn in-range?
  [range n]
  (<= (first range) n (second range)))

(defn field-ok?
  [rule n]
  (some #(in-range? % n) rule))

(defn field-ok-any-rules?
  [rules n]
  (some #(field-ok? % n) (vals rules)))

(defn fields-failing-all-rules
  [rules ticket]
  (->> ticket
     (filter #(not (field-ok-any-rules? rules %))))) 
  
(test/with-test
  (defn part1 [input]
    (let [{:keys [rules nearby-tickets]} (->> input
                                             parse-input)]
      (->> nearby-tickets
        (mapcat #(fields-failing-all-rules rules %)) 
        (reduce +))))
         

  (test/is (= 71
              (part1 test-input))))

(def test-input2
  "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9")

(defn valid-ticket?
  [rules ticket]
  (empty? (fields-failing-all-rules rules ticket)))

(defn start-column-rules
  [rules])

(defn filter-rules
  [column-rules ticket]

  (for [i (range (count ticket))
        :let [n (nth ticket i)]]
    (into {} (filter #(field-ok? (val %) n) (nth column-rules i)))))

(defn difference-rules
  [r1 r2]
  (let [k1 (set (keys r1))
        k2 (set (keys r2))
        ks (set/difference k1 k2)]
    (into {} (filter #(contains? ks (key %)) r1))))

(def data (parse-input input))
(def test-data (parse-input test-input))

(defn possible-rules
  [{:keys [rules my-ticket nearby-tickets]}]
  (let [valid-tickets (filter #(valid-ticket? rules %) nearby-tickets)
        start-rules (for [_ my-ticket] rules)]
    (reduce filter-rules start-rules valid-tickets)))

(defn clean-sorted-rules
  [acc rules]
  (if (empty? rules)
    acc
    (let [[rule & rest] rules
          new-acc (conj acc [(first rule) (first (keys (second rule)))])
          new-rules (map (fn [rs] (update rs 1 #(difference-rules % (second rule)))) rest)]
      (recur new-acc new-rules))))

(defn part2 [input]
  (let [data (parse-input input)
        {:keys [rules my-ticket nearby-tickets]} data
        prules (->> data
                    possible-rules
                    (map-indexed #(vector %1 %2))
                    (sort-by #(count (second %)))
                    (clean-sorted-rules []))]
    (->> prules
         (filter #(.startsWith (second %) "departure"))
         (map #(nth my-ticket (first %)))
         (reduce *'))))

(defn part2-alt [input]
  (let [data (parse-input input)
        {:keys [rules my-ticket nearby-tickets]} data]
    (->> rules
         (filter #(.startsWith (key %) "departure")))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

