(ns day15
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  "14,3,1,0,9,5")

(def test-input
  "0,3,6")

(defn parse-line
  [line]
  line)

(defn parse-input
  [input]
  (->> (str/split input #",")
       (map #(Integer/parseInt %))))

(defn start-state
  [numbers]
  {:turn 0 :mem {} :queue (apply list numbers) :said 0})


(defn say-number
  [state number]
  (let [{:keys [turn mem said]} state
        new_turn (inc turn)]
    (assoc state
           :turn new_turn
           :mem (update mem number #(take 2 (conj % new_turn)))
           :said number)))


(defn play-turn
  [state]
  (let [{:keys [turn mem queue said]} state
        nstate (assoc state :turn (inc turn))]
    (if (empty? queue)
      (let [number (condp = (count (mem said))
                    0 0
                    1 (- turn (first (mem said)))
                    (apply - (mem said)))]
        (say-number state number))
      (-> state
          (say-number (first queue))
          (assoc :queue (rest queue))))))
     

(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         start-state
         (iterate play-turn)
         (drop-while #(< (:turn %) 2020))
         first
         :said))

  (test/is (= 436 
              (part1 test-input))))

(test/with-test
  (defn part2 [input]
    (->> input
         parse-input
         start-state
         (iterate play-turn)
         (drop-while #(< (:turn %) 30000000))
         first
         :said))

  (test/is (= 175594 test-input (part2 test-input))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input)))


