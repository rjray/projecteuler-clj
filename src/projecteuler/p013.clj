(ns projecteuler.p013
  (:require [clojure.string :as str]))

;; https://projecteuler.net/problem=13

(defn large-sum [& [source]]
  (let [source  (or source "resources/013.txt")
        numbers (slurp source)]
    (read-string
     (str/join (take 10
                     (str (apply + (map bigint (re-seq #"\d+" numbers)))))))))
