(ns aoc24.day03
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn load-data [] (slurp "input/day03.txt"))

(defn parse1 [data]
  (map
   (fn [match] (map #(Integer/parseInt %) (rest match)))
   (re-seq #"mul\((\d+),(\d+)\)" data)))

;; thanks chatgpt
(defn re-seq-pos [pattern string]
  (let [m (re-matcher pattern string)]
    (letfn [(step []
              (when (.find m)
                (let [group-count (.groupCount m)
                      groups (map (fn [i] (.group m i)) (range (inc group-count)))]
                  (cons {:start (.start m)
                         :end (.end m)
                         :groups groups}
                        (lazy-seq (step))))))]
      (step))))


(defn find-all-indexes
  ([s value]
   (find-all-indexes s value 0 []))
  ([s value start found]
   (let [idx (str/index-of s value start)]
     (if (nil? idx)
       found
       (find-all-indexes s value (+ idx 1) (conj found idx))))))

(defn get-enabled-ranges [data]
  (loop [dos (find-all-indexes data "do()")
         donts (find-all-indexes data "don't()")
         enabled-ranges [[0 (first donts)]]]
    (let [dos-consumed (take-while #(< % (first donts)) dos)]
      (if (and (empty? dos) (empty? donts))
        enabled-ranges
        (recur
         (drop (count dos-consumed) dos)
         (rest donts)
         (if (empty? dos-consumed)
           enabled-ranges
           (conj enabled-ranges [(first dos-consumed) (first donts)])))))))

(defn get-enabled-matches [enabled-ranges matches]
  (filter
   (fn
     [match]
     (some #(< (first %) (:start match) (last %)) enabled-ranges))
   matches))

(defn parse2 [data]
  (let [enabled-ranges (get-enabled-ranges data)
        matches (re-seq-pos #"mul\((\d+),(\d+)\)" data)
        enabled-matches (get-enabled-matches enabled-ranges matches)]
    (map
     (fn [match]
       (map #(Integer/parseInt %)
            (rest (:groups match))))
     enabled-matches)))

(defn do-math [pairs]
  (apply + (map #(* (nth % 0) (nth % 1)) pairs)))

(defn part1 []
  (-> (load-data)
      parse1
      do-math))

(defn part2 []
  (-> (load-data)
      parse2
      do-math))

(part2)