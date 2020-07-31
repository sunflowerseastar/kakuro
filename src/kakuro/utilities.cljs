(ns kakuro.utilities
  (:require [tupelo.core :refer [append]]))

(defn get-square [b x y] (get (get b y) x))

(defn get-right-flag [b x y]
  (loop [x x y y]
    (let [sq (get-square b (dec x) y)]
      (if (= (:type sq) :flag) sq
          (recur (dec x) y)))))

(defn get-down-flag [b x y]
  (loop [x x y y]
    (let [sq (get-square b x (dec y))]
      (if (= (:type sq) :flag) sq
          (recur x (dec y))))))

(defn x-distance-from-summand [b x y]
  (loop [x x y y n 0]
    (let [sq (get-square b (dec x) y)]
      (if (= (:type sq) :flag) n
          (recur (dec x) y (inc n))))))

(defn y-distance-from-summand [b x y]
  (loop [x x y y n 0]
    (let [sq (get-square b x (dec y))]
      (if (= (:type sq) :flag) n
          (recur x (dec y) (inc n))))))

(defn board-solution->board-with-solutions [board solution]
  (let [x-shape (count (first board))
        board-flattened (flatten board)]
    (->> (loop [squares board-flattened acc [] n 0]
           (if (or (= n (count solution)) (empty? squares)) acc
               (let [b (first squares)]
                 (if (= (:type b) :entry)
                   (recur (rest squares) (conj acc (assoc b :value (nth solution n))) (inc n))
                   (recur (rest squares) (conj acc b) n)))))
         (partition x-shape))))

(defn board->remove-column [board]
  (map butlast board))

(defn board->add-column [board]
  (let [x-shape (-> board first count)]
    (map-indexed #(append %2 {:type :black :x x-shape :y %1}) board)))

(defn board->remove-row [board]
  (butlast board))

(defn board->add-row [board]
  (let [x-shape (-> board first count)
        new-rows (vec (for [x (range 0 x-shape)]
                        (assoc {:type :black :y (count board)} :x x)))]
    (append board new-rows)))
