(ns aoc24.day04
  (:require [clojure.string :as str]))

(defn load-data [] (str/split (slurp "input/day04.txt") #"\n"))

(defn letters-at-vector [data x y x-vector y-vector]
  (map
   (fn [i] (get-in data [(+ y (* i y-vector)) (+ x (* i x-vector))]))
   (range 4)))

(defn search-at-vector [data x y x-vector y-vector]
  (= "XMAS" (apply str (letters-at-vector data x y x-vector y-vector))))

(defn search-at-1 [data x y]
  (loop [x-vector -1 y-vector -1 found 0]
    (if (> y-vector 1)
      found
      (recur
       (if (= x-vector 1) -1 (inc x-vector))
       (if (= x-vector 1) (inc y-vector) y-vector)
       (if (search-at-vector data x y x-vector y-vector) (inc found) found)))))

(defn search-at-2 [data x y]
  (let [center (get-in data [y x])
        top-left (get-in data [(dec y) (dec x)])
        top-right (get-in data [(dec y) (inc x)])
        bottom-left (get-in data [(inc y) (dec x)])
        bottom-right (get-in data [(inc y) (inc x)])]
    (and
     (= center \A)
     (or
      (and (= top-left \M) (= bottom-right \S))
      (and (= top-left \S) (= bottom-right \M)))
     (or
      (and (= top-right \M) (= bottom-left \S))
      (and (= top-right \S) (= bottom-left \M))))))

(defn search-row [data y]
  (loop [x 0 found 0]
    (if (= x (count (get data y)))
      found
      (recur
       (inc x)
       (if (search-at-2 data x y) (inc found) found)))))

(defn search [data]
  (loop [y 0 found 0]
    (if (= y (count data))
      found
      (recur
       (inc y)
       (+ found (search-row data y))))))

(defn part2 [] (search (load-data)))

(part2)