(ns day9
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day9.txt"))

(def test-input
  "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defn parse-input
  [text]
  (->> text
       str/split-lines
       (map #(Long/parseLong %))))
       
(test/with-test
  (defn window-valid?
    [window]
    (let [len(count window)]
      (if (< len 3)
        nil
        (let [n  (first window)
              sum (last window)
              target (- sum n)
              terms (set (drop 1 (take (- len 1) window)))]
          (if (contains? terms target)
            true
            (recur (rest window)))))))
  
  (test/is (window-valid? [35 20 15 25 47 40]))
  (test/is (not (window-valid? [95 102 117 150 182 127]))))

(defn first-invalid-window
  [preamble numbers]
  (let [len (count numbers)]
    (if (<= len preamble)
      nil
      (let [window (take (+ preamble 1) numbers)]
        (if (not (window-valid? window))
          window
          (recur preamble (rest numbers)))))))

(test/with-test
  (defn part1 [preamble input]
    (->> input
         parse-input
         (first-invalid-window preamble)
         last))

  (test/is (= 127 (part1 5 test-input))))

(def invalid-number (part1 25 input))


(defn reduce-numbers
  [sum numbers]
  (reverse
   (drop-while #(> % sum) (reverse numbers))))

(defn window-with-sum
  [sum nums]
  (let [numbers (reduce-numbers sum nums)]
    (do
      (println sum (count numbers))
      (if (empty? numbers)
        nil
        (let [n (first numbers)
              remaining (- sum n)]
          (if (zero? remaining)
            #{n}
            (if-let [found (window-with-sum remaining (rest numbers))]
              (cons n found)
              (recur sum (rest numbers)))))))))           

(defn smallest-sum-not-smaller
  [maximum numbers]
  (->> numbers
       (reductions +)
       (drop-while #(> maximum %))
       first))

(defn minmax
  [numbers]
  [(apply min numbers) (apply max numbers)])

(defn start-of-contiguous-run
  [maximum numbers]
  (let [sum (smallest-sum-not-smaller maximum numbers)]
    (if (= sum maximum)
      (->> numbers
           (map vector (reductions + numbers))
           (filter #(>= maximum (first %)))
           (map second)
           minmax)
      (recur maximum (rest numbers)))))


(test/with-test
  (defn part2 [target input]
    (->> input
         parse-input
         (start-of-contiguous-run target)
         (reduce +)))

  (test/is (= 62 (part2 127 test-input))))

(comment
  (test/run-tests)
  (part1 25 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))
