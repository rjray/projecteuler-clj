(ns projecteuler.p051
  (:require [projecteuler.core :refer [primes is-prime?]]
            [clojure.string :as str]))

;; https://projecteuler.net/problem=51

;; Adaptation of Python code/algorithm found here:
;; http://blog.dreamshire.com/project-euler-51-solution/

(defn- is-a-prime? [s]
  (and (not= (first s) \0)
       (is-prime? (Integer/parseInt s))))

(defn- is-eight-prime-family? [ps c]
  (= 8
     (count (filter is-a-prime?
                    (map #(str/replace ps c %) (seq "0123456789"))))))

(defn- has-n [s c n]
  (= n (count (filter #(= c %) (seq s)))))

(defn- is-candidate? [ps]
  (let [last-digit (last ps)]
    (or (and (has-n ps \0 3)
             (is-eight-prime-family? ps \0))
        (and (has-n ps \1 3)
             (not= \1 last-digit)
             (is-eight-prime-family? ps \1))
        (and (has-n ps \2 3)
             (is-eight-prime-family? ps \2)))))

(defn prime-digit-replacements []
  (first (filter is-candidate? (map str (drop-while #(< % 100000) primes)))))
