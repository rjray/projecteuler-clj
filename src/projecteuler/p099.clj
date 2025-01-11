(ns projecteuler.p099)

;; https://projecteuler.net/problem=99

(defn- pair-to-bigint [base exp]
  (.pow (BigInteger. base) (Integer/parseInt exp)))
(defn largest-exponential-naive [& [source]]
  (let [source (or source "resources/099.txt")
        data   (slurp source)
        seq    (re-seq #"(\d+),(\d+)" data)]
    (last (last (sort #(compare (first %1) (first %2))
                      (map-indexed #(list (apply pair-to-bigint (rest %2))
                                          (inc %1))
                                   seq))))))
(defn largest-exponential-naive-parallel [& [source]]
  (let [source (or source "resources/099.txt")
        data   (slurp source)
        seq    (re-seq #"(\d+),(\d+)" data)]
    (last (last (sort #(compare (first %1) (first %2))
                      (pmap #(list (apply pair-to-bigint (rest %)) (first %))
                            (map-indexed #(cons (inc %1) (rest %2)) seq)))))))

(defn- pair-to-expnum [base exp]
  (* (Integer/parseInt exp) (Math/log10 (Integer/parseInt base))))
(defn largest-exponential [& [source]]
  (let [source (or source "resources/099.txt")
        data   (slurp source)
        seq    (re-seq #"(\d+),(\d+)" data)]
    (last (last (sort #(compare (first %1) (first %2))
                      (map-indexed #(list (apply pair-to-expnum (rest %2))
                                          (inc %1))
                                   seq))))))
