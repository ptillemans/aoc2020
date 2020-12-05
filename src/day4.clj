(ns day4
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.edn :as edn]))

(def test-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def input (slurp "./input/day4.txt"))
(def required-keys #{:day4/ecl :day4/byr :day4/iyr :day4/hgt :day4/pid  :day4/hcl :day4/eyr})

(def eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::byr (s/and int? #(<= 1920 % 2002)))
(s/def ::iyr (s/and int? #(<= 2010 % 2020)))
(s/def ::eyr (s/and int? #(<= 2020 % 2030)))
(s/def ::hgt (s/and string?
                    #(if-let [matches (re-seq #"^([\d]+)(in|cm)$" %)]
                       (let [[_dummy slen unit] (first matches)
                             len (Integer/parseInt slen)]
                         (if (= unit "cm")
                           (<= 150 len 193)
                           (<= 59 len 76))))))
(s/def ::hcl (s/and string? #(re-find #"^#[0-9a-f]{6}$" %)))
(s/def ::ecl #(contains? eye-colors %))
(s/def ::pid #(re-find #"^[0-9]{9}$" %))
(s/def ::cid string?)
(s/def ::passport
  (s/keys :req [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
          :opt [::cid]))


(defn split-cards
  [input]
  (->> (str/split input #"\n\n")
       (map parse-card)))

(defn parse-card [card]
    (->> (str/split card #"[\n ]")
       (mapcat #(let [[k v] (str/split % #":")] [(keyword (str *ns*) k) v]))
       (apply hash-map)
       (parse-field ::byr)
       (parse-field ::eyr)
       (parse-field ::iyr)))

(defn parse-field
  [key m]
  (if (contains? m key)
    (update m key edn/read-string)
    m))

(defn valid? [passport]
  (let [fields (set (keys passport))]
    (and
      (->> required-keys 
           (every? #(contains? fields %)))
      (->> fields
           (remove #(= ::cid %))
           (every? #(contains? required-keys %))))))


(test/with-test
  (defn part1
    [data]
    (->> data
         split-cards
         (filter valid?)
         count))

  (test/is (= 2 (part1 test-input))))
         
(def bad-passports "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(def good-passports "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(test/with-test
  (defn part2
    [data]
    (->> data
         split-cards
         (filter #(s/valid? ::passport %))
         count))

  (test/is (= 0 (part2 bad-passports)))
  (test/is (= 4 (part2 good-passports))))
         

(comment
  (part1 input)
  (part2 input))
