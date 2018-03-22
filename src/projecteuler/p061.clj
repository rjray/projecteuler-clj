(ns projecteuler.p061)

;; https://projecteuler.net/problem=61

;; Based (very) loosely on the algortihm in Python at
;; http://blog.dreamshire.com/project-euler-61-solution/

;; The figurate functions F(i) for i in (3 .. 8)
(defn- Tri [n] (/ (* n (inc n)) 2))
(defn- Squ [n] (* n n))
(defn- Pen [n] (/ (* n (dec (* 3 n))) 2))
(defn- Hex [n] (* n (dec (* 2 n))))
(defn- Hep [n] (/ (* n (- (* 5 n) 3)) 2))
(defn- Oct [n] (* n (- (* 3 n) 2)))

;; Are the numbers a and b cyclical?
(defn- cyclical [a b]
  (= (rem a 100) (quot b 100)))

;; For a given figurate function, return the list of 4-digit values that can be
;; part of a cyclical chain (that is, digit 3 cannot be 0)
(defn- candidates [f]
  (filter #(> (rem % 100) 9)
          (filter #(> % 1000)
                  (take-while #(< % 10000) (map f (iterate inc 1))))))

;; A map of the figurates to their indices
(def figurates {3 Tri, 4 Squ, 5 Pen, 6 Hex, 7 Hep, 8 Oct})

;; Generate all the [i, n] pairs for all the candidates
(defn- gen-all-pairs [which]
  (apply concat (for [i which] (map #(vector i %) (candidates (figurates i))))))

;; Get the semi-cyclical pairs for the given input pair p
(defn- get-cycl-pairs [p pairs]
  (filter #(and (not= (first p) (first %)) (cyclical (last p) (last %)))
          pairs))

;; Create the mappings of each pair p to the other pairs it is cyclical with
(defn- gen-mappings [pairs]
  (apply merge (for [p pairs] (hash-map p (get-cycl-pairs p pairs)))))

;; Search for a solution from the vantage point of a given partial solution
(defn- search-solution [types data length mappings]
  (cond
    (and (= (count types) length)
         (cyclical (last (first data)) (last (last data))))
    (prn (apply + (map last data)) (reverse data))
    :else
    (let [all-paths   (mappings (first data))
          valid-paths (filter #(not (types (first %))) all-paths)]
      (remove empty?
              (map #(search-solution (conj types (first %))
                                     (cons % data)
                                     length
                                     mappings) valid-paths)))))

(defn- find-cyclical-figurates [which]
  (let [pairs    (gen-all-pairs which)
        mappings (gen-mappings pairs)]
    (remove empty?
            (map #(search-solution #{ (first %) }
                                   (list %)
                                   (count which)
                                   mappings)
                 pairs))))

(defn cyclical-figurates [& [which]]
  (let [which (or which (keys figurates))]
    (find-cyclical-figurates which)))
