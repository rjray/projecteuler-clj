;;; I/O utilities for Project Euler solutions. Anything that deals with reading
;;; data or prepping freshly-read data for processing.

(ns projecteuler.io
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn into-matrix
  "Turn a sequence of sequences (seq or str types)  into a vector of vectors."
  [seqs]
  (mapv vec seqs))

(defn read-data
  "Read in the content of the given puzzle-file and return as a blob"
  [puzzle]
  (slurp (if (str/starts-with? puzzle "/") puzzle (io/resource puzzle))))

(defn to-lines
  "Turn a blob or block into a seq of lines"
  [input]
  (str/split-lines input))

(defn to-matrix
  "Turn a blob (or block) into a vector of vectors"
  [input]
  (->> input
       to-lines
       into-matrix))

(defn parse-out-longs
  "Parse out all numbers in `line` that are integers (longs)"
  [line]
  (map parse-long (re-seq #"[-+]?\d+" line)))
