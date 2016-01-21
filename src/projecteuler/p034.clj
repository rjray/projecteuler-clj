(ns projecteuler.core)

;; https://projecteuler.net/problem=34

(defn digit-factorials-sum []
  (let [fact-map (apply hash-map
                        (flatten (map #(list (char (+ % 48)) (! %))
                                      (range 10))))
        sum      (fn [a] (apply + (map fact-map (str a))))]
    (apply + (filter #(= % (sum %)) (range 10 1000000)))))
