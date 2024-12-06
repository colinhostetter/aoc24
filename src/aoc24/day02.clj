(ns aoc24.day02
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn load-data []
  (let [text-lines (-> (slurp "input/day02.txt")
                       (str/trim)
                       (str/split #"\n"))]
    (->> text-lines
         (map #(str/split % #" "))
         (map (fn [line] (into [] (map #(Integer/parseInt %) line))))
         (into []))))

(defn safe? [level]
  ;; (println "safe?" level)
  (let [changes (map-indexed
                 (fn [idx itm] (- itm (get level idx)))
                 (rest level))
        same-dir (or (every? pos? changes) (every? neg? changes))
        diff-size-ok (every? #(and (>= (Math/abs %) 1) (<= (Math/abs %) 3)) changes)]
    (and same-dir diff-size-ok)))

(defn part1 []
  (count (filter safe? (load-data))))

(part1)

(defn safe-with-dampener? [level]
  ;; (println "safe-with-dampener?" level)
  (let [size (count level)]
    (some #(= % true) (map
                       #(safe? (into [] (concat (take % level) (take-last (- size % 1) level))))
                       (range size)))))

(defn part2 []
  (count (or (filter safe-with-dampener? (load-data))
             (filter safe? (load-data)))))

(part2)
