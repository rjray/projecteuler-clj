(ns projecteuler.p089
  (:require [clojure/string :as str]))

;; https://projecteuler.net/problem=89

;; This code/algorithm inspired by the Perl Roman module:
;; http://search.cpan.org/~chorny/Roman/lib/Roman.pm

(def ^:private roman2arabic
  {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000})
(def ^:private roman-digit
  {1 ["I" "V"], 10 ["X" "L"], 100 ["C" "D"], 1000 ["M" "MMM"]})
(def ^:private figure (reverse (sort (keys roman-digit))))

(defn- arabic [r]
  (let [r (str/upper-case r)]
    (loop [[d & rs] r, last-d 1000, value 0]
      (let [this-d (roman2arabic d)]
        (cond
          (nil? d) value
          :else    (if (< last-d this-d)
                     (recur rs this-d (+ value this-d (* -2 last-d)))
                     (recur rs this-d (+ value this-d))))))))

(defn- roman-element [digit i v x]
  (cond
    (and (>= digit 1) (<= digit 3)) (str/join (repeat digit i))
    (= digit 4)                     (str i v)
    (= digit 5)                     v
    (and (>= digit 6) (<= digit 8)) (str/join (cons v (repeat (- digit 5) i)))
    (= digit 9)                     (str i x)
    :else                           ""))

(defn- roman [a]
  (loop [[place & fs] figure, x "", arg a, value ""]
    (cond
      (nil? place) value
      :else
      (let [digit (int (/ arg place)), [i v] (roman-digit place)]
        (recur fs i (- arg (* digit place))
               (str value (roman-element digit i v x)))))))

(defn roman-numerals [& [file]]
  (->> (or file "resources/089.txt")
       (slurp)
       (re-seq #"[MDCLXVI]+")
       (map #(- (count %) (count (roman (arabic %)))))
       (reduce +)))
