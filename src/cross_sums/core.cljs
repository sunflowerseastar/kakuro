(ns ^:figwheel-hooks cross-sums.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom create-class]]))

(defn generate-board []
  [[{:type :black} {:type :summands :right nil :down 4} {:type :summands :right nil :down 6}]
   [{:type :summands :right 3 :down nil} {:type :entry} {:type :entry}]
   [{:type :summands :right 7 :down nil} {:type :entry} {:type :entry}]])

(def game (atom {:board (generate-board)}))

(defn get-app-element []
  (gdom/getElement "app"))

(defn square-c [x y square]
  (let [type (:type square)]
    [:div.square
     {:key (str x y)
      :class type
      :style {:grid-column (+ x 1) :grid-row (+ y 1)}}
     (cond (= type :summands)
           [:div
            [:span.summand-right (:right square)]
            [:span.summand-down (:down square)]]
           (= type :entry)
           [:span.piece-container ""])]))

(defn main []
  (create-class
   {:reagent-render (fn [this]
                      (let [{:keys [board]} @game]
                        [:div.main
                         [:div.board-container
                          [:div.board
                           (map-indexed
                            (fn [y row]
                              (map-indexed
                               (fn [x square] [square-c x y square])
                               row))
                            board)]]]))}))

(defn mount [el]
  (reagent/render-component [main] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
