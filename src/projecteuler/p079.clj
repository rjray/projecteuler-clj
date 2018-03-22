(ns projecteuler.p079
  (:require [clojure.set :as sets]
            [clojure.string :as str]))

;; https://projecteuler.net/problem=79

(defn- graph-one-try [graph try]
  (loop [graph    graph
         [d & ds] (seq try)]
    (cond
      (nil? d) graph
      :else    (recur (assoc graph d (sets/union (graph d #{}) (set ds))) ds))))

(defn- graph-of-tries [all-tries]
  (loop [graph         {}
         [try & tries] all-tries]
    (cond
      (nil? try) graph
      :else      (recur (graph-one-try graph try) tries))))

;; Breadth-first search approach inspired by this blog post:
;; http://alexmic.net/password-derivation-project-euler/
(defn- enqueue-neighbors [queue neighbors path]
  (concat queue
          (mapcat #(list (list % (cons % path))) neighbors)))

(defn- find-solution [start graph target-len]
  (loop [queue (list (list start (list start)))
         solns ()]
    (cond
      (empty? queue) solns
      :else
      (let [[head & qs] queue
            [curr path] head
            neighbors   (graph curr #{})]
        (if (= target-len (count path))
          (recur qs (cons (str/reverse path) solns))
          (recur (enqueue-neighbors qs neighbors path) solns))))))

(defn- find-passcodes [graph]
  (let [universe   (keys graph)
        target-len (count universe)]
    (flatten (map #(find-solution % graph target-len) universe))))

(defn passcode-derivation [& [file]]
  (->> (or file "data/079.txt")
       (slurp)
       (re-seq #"\d\d\d")
       (distinct)
       (graph-of-tries)
       (find-passcodes)))
