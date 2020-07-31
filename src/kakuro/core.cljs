(ns ^:figwheel-hooks kakuro.core
  (:require
   [ajax.core :refer [POST]]
   [goog.dom :as gdom]
   [tupelo.core :refer [spyx]]
   [reagent.core :as reagent :refer [atom create-class]]))

(defn generate-board
  "Rows of squares. A square has :type, :x, :y, and a set of 1 or 2 :flags.
  The types are:
  - :black
  - :entry
  - :flag
  Each flag is {:direction :down|:right :sum int :distance int}"
  []
  [[{:type :black :x 0 :y 0}
    {:type :flag :x 1 :y 0 :flags #{{:direction :down :sum 4 :distance 2}}}
    {:type :flag :x 2 :y 0 :flags #{{:direction :down :sum 6 :distance 2}}}]
   [{:type :flag :x 0 :y 1 :flags #{{:direction :right :sum 3 :distance 2}}}
    {:type :entry :x 1 :y 1}
    {:type :entry :x 2 :y 1}]
   [{:type :flag :x 0 :y 2 :flags #{{:direction :right :sum 7 :distance 2}}}
    {:type :entry :x 1 :y 2}
    {:type :entry :x 2 :y 2}]])

(def board (atom (generate-board)))

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

(defn square-c [x y {:keys [flags type] :as square}]
  [:div.square
   {:class type
    :style {:grid-column (+ x 1) :grid-row (+ y 1)}}
   (cond (= type :flag)
         (->> flags
              (map (fn [{:keys [direction sum] :as flag}]
                     [:span.sum
                      {:key (str x y)
                       :class (name direction)}
                      sum])))
         (= type :entry)
         [:span.piece-container (:value square)])])

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

(defn update-board! [solution]
  (let [board-with-solutions (board-solution->board-with-solutions @board solution)]
    (reset! board board-with-solutions)))

(defn on-click-solve [flags-to-be-solved]
  (POST "http://localhost:3001/solve"
        {:headers {"content-type" "application/edn"}
         :body (str "{:flags-to-be-solved " flags-to-be-solved "}")
         :handler #(update-board! (:solution %))
         :error-handler #(.error js/console (str "error: " %))}))

(defn filter-by-type
  "Takes a row of squares, returns {:type :flag} ones."
  [type xs]
  (filter #(= (:type %) type) xs))

(defn flag-square->flags-to-be-solved
  "ex. {:type :flag, :x 1, :y 0, :flags #{{:direction :down, :sum 4, :distance 2}}} -> ([:d 1 0 4 2])"
  [{:keys [x y flags]}]
  (->> flags
       (mapcat (fn [{:keys [direction sum distance]}]
                 [(if (= direction :down) :d :r) x y sum distance]))
       vec))

(defn board->flags-to-be-solved
  [board]
  (->> board
       (mapcat (partial filter-by-type :flag))
       (mapv flag-square->flags-to-be-solved)))

(defn board->entries
  [board]
  (->> board (mapcat (partial filter-by-type :entry))))

(defn main []
  (create-class
   {:reagent-render
    (fn [this]
      [:div.main
       [:div.board-container
        [:div.board
         (map-indexed
          (fn [y row]
            (map-indexed
             (fn [x square]
               ^{:key (str x y)}
               [square-c x y square])
             row))
          @board)]]
       [:div.button-container
        [:button {:on-click #(on-click-solve (board->flags-to-be-solved @board))}
         "solve"]]])}))

(defn mount-app-element []
  (when-let [el (gdom/getElement "app")]
    (reagent/render-component [main] el)))

(mount-app-element)
