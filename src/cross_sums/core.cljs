(ns ^:figwheel-hooks cross-sums.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom create-class]]))

(defn generate-board []
  [(vec (map #(hash-map :color 'b :piece-type %1 :x %2 :y 0) ['r 'n 'b 'q 'k 'b 'n 'r] (range 0 8)))
   (vec (for [x (range 0 8)] {:color 'b :piece-type 'p :x x :y 1}))
   (vec (repeat 8 {}))
   (vec (repeat 8 {}))
   (vec (repeat 8 {}))
   (vec (repeat 8 {}))
   (vec (for [x (range 0 8)] {:color 'w :piece-type 'p :x x :y 6}))
   (vec (map #(hash-map :color 'w :piece-type %1 :x %2 :y 7) ['r 'n 'b 'q 'k 'b 'n 'r] (range 0 8)))])

(def game (atom {:board (generate-board)}))

(defn get-app-element []
  (gdom/getElement "app"))

(defn main []
  (create-class
   {:reagent-render (fn [this]
                      (let [{:keys [board]} @game]
                        [:div.chess
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
