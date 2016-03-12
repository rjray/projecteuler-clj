(ns projecteuler.p062)

;; https://projecteuler.net/problem=62

(defn- make-key [n]
  (apply str (sort (seq (str n)))))

(defn- add-to-cube-map [n cmap]
  (let [k (make-key n)
        v (get cmap k [])]
    (assoc cmap k (conj v n))))

(defn- get-longest-list [cmap]
  (->> (keys cmap)
       (sort #(compare (count (cmap %2)) (count (cmap %1))))
       (first)
       (cmap)))

(defn cubic-permutations [& [targetlen]]
  (let [targetlen (or targetlen 5)]
    (loop [n 345, cmap {}]
      (let [n3   (* n n n)
            cmap (add-to-cube-map n3 cmap)
            v    (get-longest-list cmap)]
        (cond
         (= (count v) targetlen) v
         :else                   (recur (inc n) cmap))))))
