(ns day18
  (:require [criterium.core :as crit]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.set :as set]))


(def input
  (slurp "./input/day18.txt"))

(defn tokenize
  [s]
  (->> (replace {"+" :plus "*" :mul "(" :open ")" :close}
        (-> s
            (str/replace "(" "( ")
            (str/replace ")" " )")
            (str/split #" ")))
       (map #(if (keyword? %) % (Integer/parseInt %)))))

(test/with-test
  ;; expr ::= value [ operand value ]*
  ;; value ::= [digit*] | '(' expr ')'
  (defn eval-expression
    ([tokens ll f]
     (if (empty? tokens)
       [(f ll) []]

       (let [[token & expr] tokens
             token (first tokens)]
          (condp = token
            :plus (eval-expression expr ll +)  
            :mul (eval-expression expr ll *) 
            :close [(f ll) (vec expr)]
            :open (let [[r ts] (eval-expression expr 0 +)]
                    (eval-expression ts (f ll r) +))
            (eval-expression expr (f ll token) +)))))

    ([s]
     (eval-expression (tokenize s) 0 +)))
  
  (test/is (= [1 []] (eval-expression "1")))
  (test/is (= [3 []] (eval-expression "1 + 2")))
  (test/is (= [6 []] (eval-expression "2 * 3")))
  (test/is (= [10 []] (eval-expression "2 * (3 + 2)")))
  (test/is (= [71 []] (eval-expression "1 + 2 * 3 + 4 * 5 + 6")))
  (test/is (= [51 []] (eval-expression "1 + (2 * 3) + (4 * (5 + 6))")))
  (test/is (= [26 []] (eval-expression "2 * 3 + (4 * 5)"))) 
  (test/is (= [437 []] (eval-expression "5 + (8 * 3 + 9 + 3 * 4 * 3)"))) 
  (test/is (= [12240 []] (eval-expression "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))) 
  (test/is (= [13632 []] (eval-expression "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")))) 


(test/with-test
  (defn part1 [input]
    (->> input
         str/split-lines
         (map eval-expression)
         (map first)
         (reduce +)))

  (test/is (= (part1 test-input))))


;; expr ::= factor [:mul factor]*
;; factor ::= term [:plus term]
;; term ::= [digit*] | '(' expr ')'
;;
(declare eval-expr)

(defn eval-term
  [term]
  (let [[token & expr] term]
    (condp = token
      :open (let [[term tokens] (eval-expr expr)]
              [term (rest tokens)])
        [token expr])))


(defn eval-factor
  [factor]
  (let [[term tokens] (eval-term factor) 
        token (first tokens)]
    (condp = token
      :plus (update (eval-factor (rest tokens)) 0 #(+ term %))
      [term tokens])))


(defn eval-expr
  [expr]
  (let [[factor tokens] (eval-factor expr)
        token (first tokens)]
    (condp = token
      :mul (let [rslt (eval-expr (rest tokens))]
              (update rslt 0 #(* factor %)))
      [factor tokens])))

(test/with-test
  (defn eval-expression2
    [s]
    (first (eval-expr (tokenize s))))
  
  (test/is (= 1  (eval-expression2 "1")))
  (test/is (= 3  (eval-expression2 "1 + 2")))
  (test/is (= 6  (eval-expression2 "2 * 3")))
  (test/is (= 10  (eval-expression2 "2 * 3 + 2")))
  (test/is (= 21 (eval-expression2 "1 + 2 * 3 + 4")))
  (test/is (= 231  (eval-expression2 "1 + 2 * 3 + 4 * 5 + 6")))
  (test/is (= 51  (eval-expression2 "1 + (2 * 3) + (4 * (5 + 6))")))
  (test/is (= 46  (eval-expression2 "2 * 3 + (4 * 5)"))) 
  (test/is (= 1445  (eval-expression2 "5 + (8 * 3 + 9 + 3 * 4 * 3)"))) 
  (test/is (= 669060 (eval-expression2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))) 
  (test/is (= 23340 (eval-expression2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")))) 

(defn part2 [input]
  (->> input
       str/split-lines
       (map eval-expression2)
       (reduce +)))


(comment
  (test/run-tests)
  (part1 input)
  (part2 input)
  (crit/bench (part1 input))
  (crit/bench (part2 input)))

