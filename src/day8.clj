(ns day8
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day8.txt"))

(def test-input
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")


(defn parse-line
  [line]
  (let [[k v] (str/split line #" ")
        value (Integer/parseInt v)]
    [(keyword k) value]))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map parse-line)
       vec))

(def start-state {:pc 0, :acc 0})


(test/with-test
  (defn interprete
    [instruction state]
    (let [cmd (first instruction)
          arg (second instruction)]
      (condp = cmd
          :nop (update state :pc inc)
          :acc (-> state
                   (update :pc inc)
                   (update :acc #(+ arg %)))
          :jmp (update state :pc #(+ arg %)))))

  (test/is (= {:pc 1 :acc 0}
              (interprete [:nop 99] start-state)))
  (test/is (= {:pc 1 :acc 7}
              (interprete [:acc 7] start-state)))
  (test/is (= {:pc 8 :acc 0}
              (interprete [:jmp 8] start-state))))


(defn log-location
  [state]
  (let [pc (:pc state)
        log (state :log #{})]
    (assoc state :log (conj log pc))))

(defn interprete-with-log
  [instruction state]
  (interprete instruction (log-location state)))

(defn run-program-with-log
  [program]
  (let [end (len program)]
    (->> start-state
         (iterate #(interprete-with-log (nth program (:pc %)) %))
         (take-while #(not (contains? (:log %) (:pc %))))
         (take-while #(< (:pc %) end)))))
         

(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         run-program-with-log
         last))
      
         

  (test/is (= 5 (part1 test-input))))

(defn is-jump?
  [[cmd _arg]]
  (= cmd :jmp))

(test/with-test
  (defn part2 [input]
    (let [program (parse-input input)
          end (count program)]
      (->> (range end)
           (filter #(is-jump? (nth program %)))
           (map #(assoc program % [:nop 0]))
           (map #(last (run-program-with-log %))))))
  (comment
           (drop-while #(< end (:pc %)))
           first)
      

  (test/is (= 8 (:acc (part2 test-input)))))

(comment
    (test/run-tests)
    (part1 input)
    (part2 test-input)
    (part2 input)
    (crit/bench (part1 input))
    (crit/bench (part2 input)))
 
