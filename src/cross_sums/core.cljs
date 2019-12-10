(ns ^:figwheel-hooks cross-sums.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom create-class]]))

(defn generate-board []
  [
   (vec (repeat 3 {}))
   (vec (repeat 3 {}))
   (vec (repeat 3 {}))
   ])

(def game (atom {:board (generate-board)}))

(defn get-app-element []
  (gdom/getElement "app"))

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
                               (fn [x square]
                                 (let [{:keys [color piece-type]} square]
                                   [:div.square
                                    {:key (str x y)
                                     :class [(if (or (and (even? y) (odd? x)) (and (odd? y) (even? x))) "dark")]
                                     :style {:grid-column (+ x 1) :grid-row (+ y 1)}}
                                    (if (not-empty square)
                                      [:span.piece-container
                                       {:class [color piece-type]} (:h1 "X")])]))
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
