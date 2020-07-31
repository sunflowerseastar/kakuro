(ns kakuro.utilities)

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
