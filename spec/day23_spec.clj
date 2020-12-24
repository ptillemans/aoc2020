(ns day23-spec
  (:require [speclj.core :refer :all]
            [day23 :refer :all]))

(def test-input
  "389125467")

(describe
 "input parser"

 (it "generates the initial cups"
     (should= [3 8 9 1 2 5 4 6 7] (parse-input test-input)))

 (it "assigns the first cup as the current cup"
     (should= 3 (first (parse-input test-input)))))

(def test-state (parse-input test-input))

(describe
 "cup calculations"

 (it "can decrement normally"
     (should= 7 (cup-dec 8)))

 (it "wraps around when at lowest value"
     (should= 9 (cup-dec 1)))

 (it "keeps going till it finds one not in the exclude list"
     (should= 7 (cup-dec 2 :exclude #{8 9 1}))))

(describe
 "take 3 cups"
 (let [na (atom (next-array test-state))]

   
   (it "takes 3 cups clockwise from current"
      (before (reset! na (next-array test-state)))
      (should= [8 9 1]
               (take-3-cups @na 3))

      (should= [5 4 6]
               (take-3-cups @na 2))

      (should= [7 3 8]
               (take-3-cups @na 6)))))

;; (describe "round"
;;  (before (reset! na (next-array test-state)))
;;  (it "returns the next cup to start from"
;;      (should= 2 (round @na 3))))

(describe
 "game"
 (it "should play n rounds starting from start"
     (should= [1 9 2 6 5 8 3 7 4]
              (game 10 test-state))))

(describe
 "format-cups"
 (it "should be a string formatted according to rules"
     (should= "92658374" (format-cups [8 3 7 4 1 9 2 6 5]))))

(describe
 "part1"
 (it "should solve part1 for example input"
     (should= "67384529" (format-cups (game 100 test-state)))))

(describe
 "part2 cups"
 (it "should return 10^6 numbers"
     (should= 1000000 (count (cups2 test-state))))
 (it "should start with the initial bnumbers"
     (should= test-state (take (count test-state) (cups2 test-state))))
 (it "should have 1000000 as maximum"
     (should= 1000000 (apply max (cups2 test-state))))
 (it "should have 1000000 different numbers"
     (should= 1000000 (count (set (cups2 test-state))))))

(run-specs)
