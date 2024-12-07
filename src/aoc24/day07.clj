(ns aoc24.day07
  [:require [clojure.string :as str]]
  [:require [clojure.math.combinatorics :as combo]])

(defn load-data []
  (->> (str/split (slurp "input/day07.txt") #"\n")
       (map #(str/split % #": "))
       (map (fn [row]
              {:result (bigint (first row))
               :nums (map
                      #(bigint %)
                      (str/split (second row) #" "))}))))

(defn get-op-permutations [ops n]
  (combo/permuted-combinations
   (into [] (apply concat (map #(repeat n %) ops)))
   n))

(defn solves? [equation ops]
  (loop [i 1 result (first (:nums equation))]
    (if (= i (count (:nums equation)))
      (if (= (:result equation) result)
        ops
        nil)
      (recur (inc i)
             ((nth ops (dec i))
              result
              (nth (:nums equation) i))))))

(defn || [a b]
  (bigint (+ b (* a (Math/pow 10 (+ 1 (Math/floor (Math/log10 b))))))))

(defn solvable? [equation]
  (some (fn [ops] (solves? equation ops))
        (get-op-permutations [+ * ||] (dec (count (:nums equation))))))

(defn part2 []
  (->> (load-data)
       (filter solvable?)
       (map :result)
       (reduce +)))

(part2)