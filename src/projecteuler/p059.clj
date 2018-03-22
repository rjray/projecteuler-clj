(ns projecteuler.p059
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

;; https://projecteuler.net/problem=59

(defn- msg-to-str [msg] (str/join (map char msg)))

(defn- xor-decrypt [ks msg]
  (let [kv (vec ks), kl (count ks)]
    (loop [idx 0, [e & es] msg, data ()]
      (if (nil? e)
        (reverse data)
        (recur (mod (inc idx) kl) es (cons (bit-xor e (kv idx)) data))))))

(defn- xor-decrypt-str [k msg]
  (xor-decrypt (map int (seq k)) msg))

(defn- derive-key-ints [n msg]
  (->> msg
       (filter #(and (>= % 65) (<= % 90)))
       (group-by identity)
       (reduce-kv (fn [c k v] (cons (list k (count v)) c)) ())
       (sort #(compare (second %2) (second %1)))
       (take n)
       (map #(bit-xor 32 (first %)))))

(defn- evaluate-potential-key [key msg]
  (let [decoded    (xor-decrypt key msg)
        value      (reduce + decoded)
        as-str     (msg-to-str decoded)
        the-count  (count (re-seq #"\bthe\b" (str/lower-case as-str)))
        key-as-str (str/join (map char key))]
    {:key       key-as-str
     :text      as-str
     :value     value
     :the-count the-count}))

(defn- decrypt-data [keylen msg]
  (->> (derive-key-ints keylen msg)
       (comb/permutations)
       (map #(evaluate-potential-key % msg))
       (sort #(compare (:the-count %2) (:the-count %1)))
       (first)))

(defn xor-decryption [& [file keylen]]
  (->> (or file "data/059.txt")
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (decrypt-data (or keylen 3))))
