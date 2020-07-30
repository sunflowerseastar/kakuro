(ns ^:figwheel-hooks cross-sums.core
  (:require
   [goog.dom :as gdom]
   [tupelo.core :refer [spyx]]
   [reagent.core :as reagent :refer [atom create-class]]))

(defn generate-board []
  [[{:type :black}
    {:type :flag :down 3}
    {:type :flag :down 5}
    {:type :flag :down 1}]
   [{:type :flag :right 3} {:type :entry} {:type :entry} {:type :entry}]
   [{:type :flag :right 6} {:type :entry} {:type :entry} {:type :entry}]])

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

(defn get-right-flag-2 [b x y]
  (loop [x x y y n 0]
    (let [sq (get-square b (dec x) y)]
      (if (= (:type sq) :flag)
        ;; (update-in sq [:right-options] #(map (fn [x] (get x 1)) %))
        (set (map #(get % n) (:right-options sq)))
        (recur (dec x) y (inc n))))))

(defn get-down-flag-2 [b x y]
  (loop [x x y y n 0]
    (let [sq (get-square b x (dec y))]
      (if (= (:type sq) :flag)
        (set (map #(get % n) (:down-options sq)))
        (recur x (dec y) (inc n))))))

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

(defn square-c [x y square]
  (let [type (:type square)]
    [:div.square
     {:class type
      :style {:grid-column (+ x 1) :grid-row (+ y 1)}}
     (cond (= type :flag)
           [:div
            [:span.summand-right (:right square)]
            [:span.summand-down (:down square)]]
           (= type :entry)
           [:span.piece-container (:value square)])]))

(defn main []
  (create-class
   {:reagent-render (fn [this]
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
                          @board)]]])}))

(defn mount-app-element []
  (when-let [el (gdom/getElement "app")]
    (reagent/render-component [main] el)))

(mount-app-element)

(defn ^:after-load on-reload []
  ;; (mount-app-element)
  )
