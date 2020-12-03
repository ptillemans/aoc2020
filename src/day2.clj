(ns day2
  (:require [clojure.string :as str]
            [clojure.test :as test]
            [criterium.core :as crit]))


(test/with-test
  (defn parse-line [line]
    (->
     (zipmap [:min :max :letter :password]
             (str/split line #"[: -]+"))
     (update :min #(Integer/parseInt %))
     (update :max #(Integer/parseInt %))
     (update :letter first)))
     

  (test/is (= {:letter \a :min 1 :max 3 :password "abcde"}
            (parse-line "1-3 a: abcde")))
  (test/is (= {:letter \b :min 1 :max 3 :password "cdefg"}
            (parse-line "1-3 b: cdefg")))
  (test/is (= {:letter \c :min 2 :max 9 :password "ccccccccc"}
            (parse-line "2-9 c: ccccccccc"))))

(test/with-test
  (defn sled-conform? [{:keys [letter min max password]}]
    (let [between-min-max? #(<= min % max)]
      (->> password
         (filter #(= letter %))
         count
         between-min-max?)))
  

  (test/is (sled-conform? (parse-line "1-3 a: abcde")))
  (test/is (not (sled-conform? (parse-line "1-3 b: cdefg"))))
  (test/is (sled-conform? (parse-line "2-9 c: ccccccccc"))))


(def data (->> (slurp "input/day2.txt")
               (str/split-lines)
               (map parse-line)))

(def test-data (->> "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
                    (str/split-lines)
                    (map parse-line)))


(test/with-test
  (defn part1 [data]
    (->> data
       (filter sled-conform?)
       (count)))
  
  (test/is (= 2 (part1 test-data))))



(test/with-test
  (defn toboggan-conform? [{:keys [letter min max password]}]
    (let [l1 (nth password (- min 1))
          l2 (nth password (- max 1))]
      (and (or (= l1 letter) (= l2 letter))
           (not (= l1 l2))))) 

  (test/is (toboggan-conform? (parse-line "1-3 a: abcde")))
  (test/is (not (toboggan-conform? (parse-line "1-3 b: cdefg"))))
  (test/is (not (toboggan-conform? (parse-line "2-9 c: ccccccccc")))))

(test/with-test
  (defn part2 [data]
    (->> data
       (filter toboggan-conform?)
       (count)))
  
  (test/is (= 1 (part2 test-data))))


(comment
  (test/run-tests)
  (part1 data)
  (part2 data)
  (crit/bench (part1 data))
  (crit/bench (part2 data)))
  
  
