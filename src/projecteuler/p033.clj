(ns projecteuler.p033
  (:require [projecteuler.core :refer [num-map]]))

;; https://projecteuler.net/problem=33

(defn- is-dcf? [[n d]]
  (let [val     (/ n d)
        [n1 n2] (map num-map (str n))
        [d1 d2] (map num-map (str d))]
    (cond
     (= n1 d2) (= val (/ n2 d1))
     (= n2 d1) (= val (/ n1 d2))
     :else false)))

;; Avg elapsed time: 10.456ms (Mac/ventrue)
;; (defn- gather-digit-cancelling-fractions []
;;   (filter #(not (nil? %))
;;           (for [n (filter #(pos? (rem % 10)) (range 11 100))
;;                 d (filter #(pos? (rem % 10)) (range (inc n) 100))]
;;             (if (is-dcf? n d) (/ n d)))))

;; Avg elapsed time: 12.470ms (Mac/ventrue)
(defn- gather-digit-cancelling-fractions []
  (map #(apply / %)
       (filter is-dcf?
               (for [n (filter #(pos? (rem % 10)) (range 11 100))
                     d (filter #(pos? (rem % 10)) (range (inc n) 100))]
                 (list n d)))))

(defn digit-cancelling-fractions []
  (reduce * (gather-digit-cancelling-fractions)))
