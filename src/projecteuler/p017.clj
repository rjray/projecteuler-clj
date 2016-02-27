(ns projecteuler.p017
  (:require [clojure.string :as str]))

;; https://projecteuler.net/problem=17

(def ^:private num-word-map
  {1 "one", 2 "two", 3 "three", 4 "four", 5 "five", 6 "six", 7 "seven",
   8 "eight", 9 "nine", 10 "ten", 11 "eleven", 12 "twelve", 13 "thirteen",
   14 "fourteen", 15 "fifteen", 16 "sixteen", 17 "seventeen", 18 "eighteen",
   19 "nineteen", 20 "twenty", 30 "thirty", 40 "forty", 50 "fifty", 60 "sixty",
   70 "seventy", 80 "eighty", 90 "ninety", 100 "hundred", 1000 "thousand"})

(defn- number-letter-count [s]
  (count (str/replace s #" " "")))

(defn- number-to-words [n]
  (cond
   (< n 21)
       (num-word-map n)
   (< n 100)
       (let [d1 (quot n 10), d2 (rem n 10), tens (* d1 10)]
         (cond
          (zero? d2) (num-word-map tens)
          :else      (str/join " " (list (num-word-map tens)
                                         (num-word-map d2)))))
   (< n 1000)
       (let [d1 (quot n 100), drest (rem n 100)]
         (cond
          (zero? drest) (str/join " " (list (num-word-map d1)
                                            (num-word-map 100)))
          :else         (str/join " " (list (num-word-map d1)
                                            (num-word-map 100)
                                            "and"
                                            (number-to-words drest)))))
   :else
       (let [d1 (quot n 1000), drest (rem n 1000)]
         (cond
          (zero? drest) (str/join " " (list (num-word-map d1)
                                            (num-word-map 1000)))
          :else         (str/join " " (list (num-word-map d1)
                                            (num-word-map 1000)
                                            (number-to-words drest)))))))

(defn number-letter-counts [& [n]]
  (let [n (or n 1000)]
    (reduce + (map number-letter-count (map number-to-words
                                            (range 1 (inc n)))))))
