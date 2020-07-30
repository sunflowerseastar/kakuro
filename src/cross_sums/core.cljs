(ns ^:figwheel-hooks cross-sums.core
  (:require
   [goog.dom :as gdom]
   [tupelo.core :refer [spyx]]
   [reagent.core :as reagent :refer [atom create-class]]))

(defn generate-board []
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
