(ns projecteuler.core)

;; https://projecteuler.net/problem=18

(defn- triangle [file]
  (map #(Integer/parseInt %) (re-seq #"\d\d" (slurp file))))
(defn- triangle-tree [s]
  (loop [n    1
         s    s
         tree nil]
    (cond
     (empty? s) tree
     true (recur (inc n) (drop n s) (cons (take n s) tree)))))
(defn- reduce-row [s]
  (map #(reduce max %) (partition 2 1 s)))
(defn- combine-rows [s1 s2]
  (map + (reduce-row s1) s2))
(defn maximum-path-sum [& [file]]
  (let [file (or file "data/018.txt")
        data (triangle file)
        tree (triangle-tree data)]
    (first (reduce combine-rows tree))))
