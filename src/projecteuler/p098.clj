(ns projecteuler.p098
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

;; https://projecteuler.net/problem=98

(def ^:private all-permutations
  (vec (map #(mapcat comb/permutations
                     (comb/combinations (range 10) %)) (range 10))))
(def ^:private all-squares
  (set (map str (map #(* % %) (range 1 (inc (Math/sqrt 999999999)))))))

(def ^:private not-nil? (complement nil?))

(defn- word-key [s] (str/join (sort (seq s))))

(defn- word-key-distinct [s] (str/join (distinct (sort (seq s)))))

(defn- encode-words [key perm ws]
  (let [keymap (zipmap (seq key) perm)]
    (map #(str/join (map keymap %)) ws)))

(defn- square-pair? [s]
  (if (and (all-squares (first s)) (all-squares (last s))) s))

(defn- get-words [file]
  (re-seq #"\w+" (slurp file)))

(defn- get-anagrams [words]
  (filter #(> (count %) 1) (vals (group-by word-key words))))

(defn- anagram-pairs [anagrams]
  (mapcat #(comb/combinations % 2) anagrams))

(defn- keyed-anagram-pairs [pairs]
  (group-by #(word-key-distinct (first %)) pairs))

(defn- test-one-pair [k pair]
  (let [perms (all-permutations (count k))]
    (filter not-nil? (map #(square-pair? (encode-words k % pair)) perms))))

(defn- process-pairs [k pairs]
  (filter not-nil? (map #(test-one-pair k %) pairs)))

(defn- process-keys [key-seq key-pairs-map]
  (filter not-nil? (map #(process-pairs % (key-pairs-map %)) (sort key-seq))))

(defn- find-max-square-from-pairs [keyed-pairs]
  (let [pair-keys   (keys keyed-pairs)
        keys-by-len (group-by count pair-keys)]
    (loop [n 9, acc ()]
      (let [s (flatten (process-keys (keys-by-len n) keyed-pairs))]
        (cond
         (= n 3)    (last (sort (map #(Integer/parseInt %) acc)))
         (empty? s) (recur (dec n) acc)
         :else      (recur (dec n) (concat acc s)))))))

(defn anagramic-squares [& [file]]
  (let [file (or file "resources/098.txt")]
    (->> file
         (get-words)
         (get-anagrams)
         (anagram-pairs)
         (keyed-anagram-pairs)
         (find-max-square-from-pairs))))
