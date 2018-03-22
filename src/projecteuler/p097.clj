(ns projecteuler.p097
  (:require [clojure.string :as str]))

;; https://projecteuler.net/problem=97

(defn large-non-mersenne-prime []
  (str/reverse
   (take 10
         (reverse (str (inc (* 28433 (.pow (BigInteger. "2") 7830457))))))))
