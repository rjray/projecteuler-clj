(ns projecteuler.core)

;; https://projecteuler.net/problem=48

(defn pow' [a]
  (reduce *' (repeat a a)))
(defn self-powers [& [max]]
  (let [max (or max 1000)]
    (apply str
           (reverse (take 10
                          (reverse (str (reduce +'
                                                (map pow'
                                                     (range 1
                                                            (inc max)))))))))))
