(ns day13
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day13.txt"))

(def test-input
  "939
7,13,x,x,59,x,31,19")

(defn parse-input
  [input]
  (let [[arrival & busses] (str/split input #"[\n,]")]
    {:arrival (Integer/parseInt arrival)
     :busses (->>  busses
                   (filter #(not= "x" %))
                   (map #(Integer/parseInt %)))}))

(defn add-arrival-time
  [arrival bus]
  {bus (mod (- arrival) bus)})

(defn add-wait-times
  [state]
  (let [{:keys [arrival busses]} state
        wait-times (->> busses
                        (mapcat #(add-arrival-time arrival %))
                        (into {}))]
    (assoc state :wait-times wait-times)))

(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         add-wait-times
         :wait-times
         (apply min-key second) 
         (apply (fn [bus wait] (* bus wait)))))
         
  (test/is (= 295 (part1 test-input))))

(defn parse-bus
  [index bus-str]
  (if (not= bus-str "x")
    (let [bus (Integer/parseInt bus-str)
          offset (mod (- index) bus)]
      [bus offset]))) 

(defn parse-input2
  [input]
  (->> str/split-lines input
      (filter #(contains? \. %))
      (mapcat #(str/split % #","))
      (map-indexed parse-bus)
      (filter identity)))

(defn gcd-extended
  [a b]
  (if (zero? a)
    [b 0 1]
    (let [[g x y] (gcd-extended (mod b a) a)]
       [g (- y (* (quot b a) x)) x])))  

(defn mod-inverse
  [a m]
  (let [[gcd x y] (gcd-extended a m)]
    (if (= gcd 1)
      (mod x m)
      nil)))

(defn calc-next-seq
  [a b pairs]
  (for [[ai bi] pairs
        :let [a-1 (mod-inverse a ai)]]
    [ai (mod (* a-1 (- bi b)) ai)]))

(defn recur-step
  [a b pairs out]
  (if (empty? pairs)
    (cons [a b] out) 
    (let [[a1 b1] (first pairs)
          next-pairs (calc-next-seq a1 b1 (rest pairs))]
      (recur a1 b1 next-pairs (cons [a b] out)))))

(defn reduce-step
  [acc [a b]]
  (+ (* acc a) b))

(defn calc-first-time-with-offsets
  [pairs]
  (reduce reduce-step 0 pairs))

(test/with-test
  (defn part2 [input]
    (->> input
         parse-input2
         calc-first-time-with-offsets))

  (test/is (= 1068781 (part2 test-input)))
  (test/is (= 3417 (part2 "17,x,13,19")))
  (test/is (= 754018 (part2 "67,7,59,61")))
  (test/is (= 779210 (part2 "67,x,7,59,61")))
  (test/is (= 1261476 (part2 "67,7,x,59,61")))
  (test/is (= 1202161486 (part2 "1789,37,47,1889"))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

