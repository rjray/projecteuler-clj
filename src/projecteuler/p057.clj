(ns projecteuler.p057)

;; https://projecteuler.net/problem=57

(defn- terms [d]
  (cond
   (zero? d) 0
   :else     (/ 1 (+ 2 (terms (dec d))))))

(defn square-root-convergents [& [maxd]]
  (let [maxd (or maxd 1000)]
    (count (filter #(> (count (str (numerator %)))
                       (count (str (denominator %))))
                   (map #(inc (terms %)) (range 1 (inc maxd)))))))
