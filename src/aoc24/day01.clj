(ns aoc24.day01
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.core :as core])
  (:gen-class))

(defn parse-lists [lines]
  (let [split (mapv
               (fn [line] (str/split line #"   "))
               lines)]
    (loop [i 0 left [] right []]
      (if (>= i (count split))
        {:left (sort left) :right (sort right)}
        (recur
         (inc i)
         (conj left (Integer/parseInt (get-in split [i 0])))
         (conj right (Integer/parseInt (get-in split [i 1]))))))))

(defn load-lists []
  (with-open [rdr (io/reader "input/day01.txt")]
    (parse-lists (line-seq rdr))))

(load-lists)

(defn part1
  []
  (let [lists (load-lists)
        left (into [] (get lists :left))
        right (into [] (get lists :right))]
    (loop [i 0 sum 0]
      (if (>= i (count left))
        sum
        (recur
         (inc i)
         (+ sum (Math/abs (- (get left i) (get right i)))))))))

(part1)

(defn part2
  []
  (let [lists (load-lists)
        left (into [] (get lists :left))
        right (into [] (get lists :right))]
    (loop [i 0 score 0]
      (if (>= i (count left))
        score
        (let [value (get left i)
              appearances (count (filter #(= value %) right))]
          (recur
           (inc i)
           (+ score (* value appearances))))))))

(part2)