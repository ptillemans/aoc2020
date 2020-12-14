(ns day14
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day14.txt"))

(def test-input
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(defn parse-line
  [line]
  (let [[cmd value] (str/split line #" = ")]
    (if (= cmd "mask")
      (let [and-mask (Long/parseLong (.replaceAll value "X" "1") 2)
            or-mask (Long/parseLong (.replaceAll value "X" "0") 2)]
        [:mask and-mask or-mask])
      (let [[_cmd add _dummy] (str/split cmd #"[\[\]]")]
        [:mem (Long/parseLong add) (Long/parseLong value)]))))

    
(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map parse-line)))

(def start-state {:mem {} :mask []})

(defn interprete-instruction
  [state instruction] 
  (condp = (first instruction) 
    :mask
    (assoc state :mask (rest instruction))
    :mem
    (let[[mask-and mask-or] (:mask state)
         [address raw-value] (rest instruction)
         value (bit-or mask-or (bit-and mask-and raw-value))]
      (assoc-in state [:mem address] value)))) 

(defn interprete
  [state program]
  (reduce interprete-instruction state program))
  
(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         (interprete start-state) 
         :mem
         vals
         (reduce +)))
         

  (test/is (= 165 (part1 test-input))))

(def test-input2
  "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(defn float-mask
  [mask-and mask-or]
  (bit-and mask-and (bit-not mask-or)))

(defn count-bits
  [n]
  (->> (Long/toBinaryString n)
       (filter #(= \1 %))
       count))

(def highest-address-bit (bit-shift-left 1 36))

  
(test/with-test
  (defn expand-addresses
    [mask]
    (if (zero? mask)
      (list 0)
      (let [ibit (->> (range 36)
                      (map #(bit-shift-left 1 %)) 
                      (drop-while #(zero? (bit-and mask %)))
                      first)
            nmask (bit-and mask (bit-not ibit))]
        (let [bits (expand-addresses nmask)]
            (concat
                bits
                (map #(bit-or ibit %) bits))))))


  (test/is (= [0 1] (expand-addresses 1)))
  (test/is (= [0 4] (expand-addresses 4)))
  (test/is (= #{0 1 4 5} (set (expand-addresses 5)))))
  
(defn interprete-instruction2
  [state instruction] 
  (condp = (first instruction) 
    :mask
    (assoc state :mask (rest instruction))
    :mem
    (let[[mask1 mask0] (:mask state)
         mask-float (float-mask mask1 mask0)
         mask-force (bit-and mask1 (bit-not mask-float))
         [raw-address value] (rest instruction)
         address (bit-and
                  (bit-or mask-force raw-address)
                  (bit-not mask-float))
         addresses (expand-addresses mask-float)
         float-addresses (map #(bit-or address %) addresses)]
        (reduce
            #(assoc-in %1 [:mem %2] value)
            state
            float-addresses)))) 

(defn interprete2
  [state program]
  (reduce interprete-instruction2 state program))
  
(test/with-test
  (defn part2 [input]
    (->> input
         parse-input
         (interprete2 start-state)
         :mem
         vals
         (reduce +)))

  (test/is (= 208 (part2 test-input2))))

(comment
    (test/run-tests)
    (part1 input)
    (part2 input)
    (crit/bench (part1 input))
    (crit/bench (part2 input)))

