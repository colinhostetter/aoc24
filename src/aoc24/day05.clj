(ns aoc24.day05
  [:require [clojure.string :as str]]
  [:require [clojure.set]])

(defn load-data []
  (let [raw-data (slurp "input/day05.txt")
        sections (str/split raw-data #"\n\n")
        rules-raw (first sections)
        updates-raw (last sections)
        rules (map #(str/split % #"\|") (str/split rules-raw #"\n"))
        updates (map #(str/split % #"\,") (str/split updates-raw #"\n"))]
    {:rules rules :updates updates}))

(defn pages-preceding [rules num]
  (->> rules
       (filter #(= (second %) num))
       (map first)
       set))

(defn is-page-ordered [rules update index]
  (empty? (clojure.set/intersection
           (pages-preceding rules (get update index))
           (set (drop (inc index) update)))))

(defn is-update-ordered [rules update]
  (every? #(is-page-ordered rules update %) (range (count update))))

(defn reorder [rules update]
  (loop [reordered []]
    (if (= (count reordered) (count update))
      reordered
      (let [remaining (clojure.set/difference (set update) (set reordered))
            next-page (first (filter
                              (fn [pg] (empty? (clojure.set/intersection
                                                (set (filter #(not (= pg %)) remaining))
                                                (pages-preceding rules pg))))
                              remaining))]
        (recur (conj reordered next-page))))))

(defn middle-num [s]
  (nth s (int (/ (count s) 2))))

(defn sum-strings [s]
  (reduce + (map #(Integer/parseInt %) s)))

(defn part1 []
  (let [data (load-data)
        in-order (filter #(is-update-ordered (:rules data) %) (:updates data))
        middle-nums (map middle-num in-order)]
    (sum-strings middle-nums)))

(part1)

(defn part2 []
  (let [data (load-data)
        out-of-order (filter #(not (is-update-ordered (:rules data) %)) (:updates data))
        reordered (map #(reorder (:rules data) %) out-of-order)
        middle-nums (map middle-num reordered)]
    (sum-strings middle-nums)))

(part2)
