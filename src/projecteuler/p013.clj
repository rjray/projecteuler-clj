(ns projecteuler.core)

;; https://projecteuler.net/problem=13

(defn large-sum [& [source]]
  (let [source  (or source "data/013.txt")
        numbers (slurp source)]
    (read-string
     (apply str
            (take 10
                  (str (apply + (map bigint (re-seq #"\d+" numbers)))))))))
