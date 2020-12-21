(ns day21
  (:require [clojure.string :as str])
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day21.txt"))

(def test-input "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
")


(defn parse-line
  [line]
  (let [[ingredients allergens] (str/split line #" \(contains |\)")]
    {:ingredients (set (str/split ingredients #" "))
     :allergens (set (str/split allergens #", "))}))

(defn parse-input
  [lines]
  (->> lines
       str/split-lines
       (map parse-line)
       vec))


(defn all-allergens
  [foods]
  (->> foods
       (mapcat :allergens)
       set))

(defn all-ingredients
  [foods]
  (->> foods
       (mapcat :ingredients)
       set))


(defn ingredient-safe-for-allergen?
  "determine if ingredient cannot contain allergen

  An ingredient is safe for an allergen if there exists
  a food containing the allergen which does not have the
  ingredient, because an leergen can only be in exact 1
  ingredient"
  [foods ingredient allergen]
  (->> foods
       (filter #(contains? (:allergens %) allergen))
       (filter #(not (contains? (:ingredients %) ingredient)))
       empty?
       not))

(defn ingredient-safe?
  [foods ingredient]
  (->> (all-allergens foods)
       (map #(ingredient-safe-for-allergen? foods ingredient %))
       (every? true?)))
  
(defn safe-ingredients
  [foods]
  (->> (all-ingredients foods)
       (filter #(ingredient-safe? foods %))
       set))  

(defn count-recipes-with-ingredient
  [foods ingredient]
  (->> foods
       (filter #(contains? (:ingredients %) ingredient))
       count))

(defn count-usage-safe-ingredients
  [foods]
  (->> foods
       safe-ingredients
       (map #(count-recipes-with-ingredient foods %))
       (reduce +)))
       
(test/with-test
  (defn part1 [input]
    (->> input
         parse-input
         count-usage-safe-ingredients))

  (test/is (= 5 (part1 test-input))))



(defn remove-safe-ingredients
  [foods]
  (let [si  (safe-ingredients foods)]
    (->> foods
         (map (fn[f] (update f :ingredients #(set/difference % si)))))))


(defn possible-allergens-for-ingredient
  [foods ingredient]
  (->> foods
       (filter #(contains? (:ingredients %) ingredient))
       (map :allergens)
       (reduce set/union)))

(defn possible-ingredients-for-allergen
  [foods allergen]
  (->> foods
       (filter #(contains? (:allergens %) allergen))
       (map :ingredients)
       (reduce set/intersection)))


(defn possible-allergens
  [foods]
  (->> (all-ingredients foods)
       (map #(vector % (possible-allergens-for-ingredient foods %)))
       (into {})))

(defn possible-ingredients
  [foods]
  (->> (all-allergens foods)
       (map #(vector % (possible-ingredients-for-allergen foods %)))
       (into {})))

(defn sure-pair
  [allergen-map]
  (->> allergen-map
      (filter #(= 1 (count (val %))))
      (map #(vector (key %) (first (val %))))
      first))
  
(defn remove-sure-pair
  [allergen-map [allergen ingredient]]
  (->> allergen-map
      (filter #(not= allergen (key %)))
      (map (fn [[k v]]
             [k (disj v ingredient)]))
      (into {})))

(defn clean-allergen-map
  [allergen-map]
  (if (empty? allergen-map)
    []
    (let [sp (sure-pair allergen-map)
          new-map (remove-sure-pair allergen-map sp)]
      (cons sp (clean-allergen-map new-map)))))
                     
(defn find-ingredients-for-allergen
  "the ingredient should be the ingredient "
  [foods])

(test/with-test
  (defn part2 [input]
    (->> input
         parse-input
         remove-safe-ingredients
         possible-ingredients
         clean-allergen-map
         (sort-by first)
         (map second)
         (str/join ",")))

  (test/is (= test-input (part2 test-input))))

(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

