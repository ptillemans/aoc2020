(ns day12
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day12.txt"))

(def test-input
  "F10
N3
F7
R90
F11")

(defn parse-line
  [line]
  {:cmd (keyword (.substring line 0 1))
   :value (Integer/parseInt (.substring line 1))})

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map parse-line)))

;; X --> East,  Y --> North
(def start {:x 0 :y 0 :dir :E})
(def compass [:E :S :W :N])
(defn north
  [value state]
  (update state :y #(+ % value))) 

(defn south
  [value state]
  (update state :y #(- % value))) 

(defn east
  [value state]
  (update state :x #(+ % value))) 

(defn west
  [value state]
  (update state :x #(- % value))) 

(defn move
  [dir value state]
  (condp = dir
        :N (north value state)
        :S (south value state)
        :E (east value state)
        :W (west value state)))

(defn right
  [value state]
  (let [delta (/ value 90)
        kwadrant  (.indexOf compass (:dir state))
        new-kwadrant (mod (+ kwadrant delta) 4)]
    (assoc state :dir (nth compass new-kwadrant))))

(defn left
  [value state]
  (right (- value) state))

(defn forward
  [value state]
  (move (:dir state) value state))

(def cmd-map
  {:N north :E east :W west :S south :L left :R right :F forward})
              
              
(defn apply-instruction
  [state {:keys [cmd value]}]
  (if (contains? cmd-map cmd)
    ((cmd-map cmd) value state)
    (println "Error: command " cmd " is not supported.")))

(defn abs [x] (java.lang.Math/abs x))

(defn manhattan-distance
  [{:keys [x y]}]
  (+  (abs x) (abs y)))

(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         (reduce apply-instruction start)
         manhattan-distance))

  (test/is (= 25 (part1 test-input))))

(def start2 {:x 0 :y 0 :waypoint {:x 10 :y 1}})


(defn north2
  [value state]
  (update state :waypoint #(north value %)))

(defn east2
  [value state]
  (update state :waypoint #(east value %)))

(defn south2
  [value state]
  (update state :waypoint #(south value %)))

(defn west2
  [value state]
  (update state :waypoint #(west value %)))

(test/with-test
  (defn right2
    [value state]
    (let [kwadrants (mod (/ value 90) 4)
          {:keys [x y]} (:waypoint state)]
      (condp = kwadrants
        0 state
        1 (assoc state :waypoint {:x y :y (- x)})
        2 (assoc state :waypoint {:x (- x) :y (- y)})
        3 (assoc state :waypoint {:x (- y) :y x}))))
  (test/is (= {:x 170 :y 38 :waypoint {:x 4 :y -10}}
              (right2 90 {:x 170 :y 38 :waypoint {:x 10 :y 4}}))))

(defn left2
  [value state]
  (right2 (- value) state))

(defn forward2
  [value state]
  (let [{:keys [x y waypoint]} state
        dx (* value (:x waypoint))
        dy (* value (:y waypoint))]
    (assoc state :x (+ x dx) :y (+ y dy))))

(def cmd-map2 {:N north2 :E east2 :S south2 :W west2
               :L left2 :R right2 :F forward2})

(defn apply-instruction2
  [state {:keys [cmd value]}]
  (if (contains? cmd-map cmd)
    ((cmd-map2 cmd) value state)
    (println "Error: command " cmd " is not supported.")))

  
(test/with-test
  (defn part2 [input]
    (->> input
         parse-input
         (reduce apply-instruction2 start2)
         manhattan-distance))

  (test/is (= 286 (part2 test-input))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

