(ns ^:figwheel-hooks kakuro.core
  (:require
   [ajax.core :refer [POST]]
   [goog.dom :as gdom]
   [kakuro.utilities :as util]
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

(defn reset-board! [new-board]
  (reset! board new-board))

(defn clear-board! []
  (let [clear-values (fn [squares] (->> squares (map #(assoc % :value nil))))]
    (reset-board! (->> @board (map clear-values)))))

(defn square-c [x y {:keys [flags type] :as square} click-fn]
  [:div.square
   {:class type
    :on-click #(click-fn x y square)
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

(defn request-solution [flags-to-be-solved]
  (POST "http://localhost:3001/solve"
        {:headers {"content-type" "application/edn"}
         :body (str "{:flags-to-be-solved " flags-to-be-solved "}")
         :handler #(reset-board! (util/board-solution->board-with-solutions @board (:solution %)))
         :error-handler #(.error js/console (str "error: " %))}))

(defn change-square! [x y new-type]
  (swap! board assoc-in [y x] {:type new-type :value nil}))

(defn on-click-square [x y {:keys [type]}]
  (spyx x y type)
  (cond (= type :entry)
        (change-square! x y :black)
        (= type :black)
        (change-square! x y :entry)))

(defn main []
  (letfn [(keyboard-listeners [e]
            (let [is-enter (= (.-keyCode e) 13)
                  is-c (= (.-keyCode e) 67)
                  is-s (= (.-keyCode e) 83)
                  is-up (= (.-keyCode e) 38)
                  is-down (= (.-keyCode e) 40)
                  is-left (= (.-keyCode e) 37)
                  is-right (= (.-keyCode e) 39)
                  height (count @board)
                  width (-> @board first count)]
              (cond is-c (clear-board!)
                    (or is-enter is-s) (request-solution (util/board->flags-to-be-solved @board))
                    is-up (when (> height 3) (reset-board! (util/decrease-board-size @board)))
                    is-down (when (< height 14) (reset-board! (util/increase-board-size @board)))
                    is-left (when (> width 3) (reset-board! (util/decrease-board-size @board)))
                    is-right (when (< width 14) (reset-board! (util/increase-board-size @board))))))]
    (create-class
     {:component-did-mount (fn [] (.addEventListener js/document "keydown" keyboard-listeners))
      :reagent-render
      (fn [this]
        [:div.main
         [:div.board-container
          [:div.board
           (map-indexed
            (fn [y row]
              (map-indexed
               (fn [x square]
                 ^{:key (str x y)}
                 [square-c x y square on-click-square])
               row))
            @board)]]
         [:div.button-container
          [:button {:on-click #(request-solution (util/board->flags-to-be-solved @board))}
           "solve"]]])})))

(defn mount-app-element []
  (when-let [el (gdom/getElement "app")]
    (reagent/render-component [main] el)))

(mount-app-element)
