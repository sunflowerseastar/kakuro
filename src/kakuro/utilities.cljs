(ns kakuro.utilities
  (:require [tupelo.core :refer [append spyx]]))

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

(defn get-num-entries-below [b x y]
  (loop [x x y y n 0]
    (let [sq (get-in b [(inc y) x])]
      (if (not= (:type sq) :entry) n
          (recur x (inc y) (inc n))))))

(defn get-num-entries-right [b x y]
  (loop [x x y y n 0]
    (let [sq (get-in b [y (inc x)])]
      (if (not= (:type sq) :entry) n
          (recur (inc x) y (inc n))))))

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
           (if (empty? squares) acc
               (let [square (first squares)]
                 (if (= (:type square) :entry)
                   (recur (rest squares) (conj acc (assoc square :value (nth solution n))) (inc n))
                   (recur (rest squares) (conj acc square) n)))))
         (partition x-shape)
         (map #(into [] %))
         vec)))

(defn board->remove-column [board]
  (mapv (comp vec butlast) board))

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

(defn decrease-board-size [board]
  (->> board board->remove-row board->remove-column vec))

(defn increase-board-size [board]
  (->> board board->add-row board->add-column vec))

(defn filter-by-type
  "Takes a row of squares, returns {:type :flag} ones."
  [type xs]
  (filter #(= (:type %) type) xs))

(defn possible-sum? [n]
  (< 0 n (reduce + (range 1 10))))

(defn flag-square->flags-to-be-solved
  "ex. {:type :flag, :x 1, :y 0, :flags {:down {:sum 4 :distance 2}}} -> ([:d 1 0 4 2])
  mapcat is receiving ex. [:down {:sum 4 :distance 2}]"
  [{:keys [x y flags]}]
  (->> flags
       (filter (fn [[direction {:keys [sum distance]}]]
                 (and (possible-sum? sum) (< 0 distance 9))))
       (map (fn [[direction {:keys [sum distance]}]]
              [(if (= direction :down) :d :r) x y sum distance]))))

(defn board->flags-to-be-solved
  [board]
  (->> board
       (mapcat (partial filter-by-type :flag))
       (mapcat flag-square->flags-to-be-solved)
       (filter (comp not empty?))))

(defn board->entries
  [board]
  (->> board (mapcat (partial filter-by-type :entry))))

(defn update-square-distances [square num-entries-below num-entries-right]
  (-> square
      (assoc-in [:flags :down :distance] num-entries-below)
      (assoc-in [:flags :right :distance] num-entries-right)))

(defn fix-flags
  "This function takes a board and returns a board with two modifications:
  - if a flag square is 'unused', then change it to a black
  - if a flag is used, then update/correct its distances"
  [board]
  (let [x-shape (count (first board))
        board-flattened (flatten board)]
    (->> (loop [squares board-flattened acc [] n 0]
           (if (empty? squares) acc
               (let [{:keys [type x y flags] :as square} (first squares)]
                 (if (= type :flag)
                   (let [down-flag (:down flags)
                         num-entries-below (get-num-entries-below board x y)
                         is-down-unused (zero? num-entries-below)
                         right-flag (:right flags)
                         num-entries-right (get-num-entries-right board x y)
                         is-right-unused (zero? num-entries-right)
                         new-square (update-square-distances square num-entries-below num-entries-right)]
                     (if (and is-down-unused is-right-unused)
                       ;; change [unused] flag square to black
                       (recur (rest squares) (conj acc {:type :black :x x :y y}) n)
                       ;; update flag square with correct distances
                       (recur (rest squares) (conj acc new-square) n)))
                   (recur (rest squares) (conj acc square) n)))))
         (partition x-shape)
         (map #(into [] %))
         vec)))

(defn fix-entries
  "This function takes a board and returns a board without orphaned entries."
  [board]
  (let [x-shape (count (first board))
        board-flattened (flatten board)]
    (->> (loop [squares board-flattened acc [] n 0]
           (if (empty? squares) acc
               (let [{:keys [type x y] :as square} (first squares)]
                 (if (= type :entry)
                   (let [square-above (get-in board [(dec y) x])
                         square-left (get-in board [y (dec x)])]
                     (if (or (= (:type square-above) :black) (= (:type square-left) :black))
                       ;; replace orphaned entry with black
                       (recur (rest squares) (conj acc {:type :black :x x :y y}) n)
                       (recur (rest squares) (conj acc square) n)))
                   (recur (rest squares) (conj acc square) n)))))
         (partition x-shape)
         (map #(into [] %))
         vec)))
