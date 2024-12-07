(ns aoc24.day06
  [:require [clojure.string :as str]]
  [:require [clojure.set]])

(def raw (vec (str/split (slurp "input/day06.txt") #"\n")))

(defn parse-grid [raw-vec]
  (loop [x 0 y 0 coords #{} start nil]
    (let [char (get-in raw-vec [y x])
          row-end (= (inc x) (count (first raw-vec)))]
      (if (nil? char)
        {:obstacles coords
         :start start
         :bounds [(count (first raw-vec)) (count raw-vec)]}
        (recur
         (if row-end 0 (inc x))
         (if row-end (inc y) y)
         (if (= char \#) (conj coords [x y]) coords)
         (if (= char \^) [x y] start))))))

(defn turn [dir]
  (cond
    (= dir [0 -1]) [1 0]
    (= dir [1 0]) [0 1]
    (= dir [0 1]) [-1 0]
    (= dir [-1 0]) [0 -1]
    :else (throw (Throwable. "Unexpected direction"))))

(defn move [grid loc dir]
  (let [x (first loc) y (second loc)
        next [(+ x (first dir)) (+ y (second dir))]]
    (if (contains? (:obstacles grid) next)
      {:loc loc :dir (turn dir)}
      {:loc next :dir dir})))

(defn oob? [grid loc]
  (or (>= (first loc) (first (:bounds grid)))
      (< (first loc) 0)
      (>= (second loc) (second (:bounds grid)))
      (< (second loc) 0)))

(defn walk [grid]
  (loop [loc (:start grid)
         dir [0 -1]
         visited []]
    (if
     (oob? grid loc)
      (drop-last visited)
      (let [next-step (move grid loc dir)]
        (recur
         (:loc next-step)
         (:dir next-step)
         (conj visited next-step))))))

(defn part1 []
  (->> (walk (parse-grid raw))
       (map :loc)
       set
       count))

(defn causes-loop? [grid loc]
  (println (str "checking" loc))
  (loop [visited [loc]]
    (cond
      (and (> (count visited) 2) (= (first visited) (last visited))) true
      (oob? grid (:loc (last visited))) false
      :else (recur (conj visited
                         (move grid
                               (:loc (last visited))
                               (:dir (last visited))))))))

(defn part2 []
  (let [grid (parse-grid raw)]
    (->> (walk grid)
         (filter #(not= (:loc %) (:start grid)))
         (take 10)
         (filter #(causes-loop? grid %))
         count)))
