(ns ^:figwheel-hooks kakuro.core
  (:require
   [ajax.core :refer [POST]]
   [goog.dom :as gdom]
   [kakuro.utilities :as util]
   [kakuro.boards :as boards]
   [tupelo.core :refer [spyx]]
   [reagent.core :as reagent :refer [atom create-class]]))

;; "A board contains rows of squares. A square has :type, :x, :y, and a set of 1 or 2 :flags.
;;   The types are:
;;   - :black
;;   - :entry
;;   - :flag
;;   Each flag is {:direction :down|:right :sum int :distance int}"
(def board (atom boards/b1))

;; "ui"
(def is-requesting (atom false))
(def is-timeout (atom false))
(def is-no-solution (atom false))
(def is-success (atom false))

(defn clear-ui! []
  (do (reset! is-requesting false)
      (reset! is-timeout false)
      (reset! is-no-solution false)
      (reset! is-success false)))

(defn reset-board! [new-board]
  (reset! board new-board))

(defn clear-board! []
  (let [clear-values (fn [squares] (->> squares (mapv #(assoc % :value nil))))]
    (reset-board! (->> @board
                       (map clear-values)
                       vec))))

(defn clear! []
  (do (clear-ui!)
      (clear-board!)))

(defn square-c [x y {:keys [flags type] :as square} click-fn dbl-click-fn update-sum-fn x-shape y-shape]
  [:div.square
   {:class type
    :on-click #(click-fn x y square)
    :on-double-click #(dbl-click-fn x y square)
    :style {:grid-column (+ x 1) :grid-row (+ y 1)}}
   (cond (= type :flag)
         (->> flags
              (map (fn [[direction {:keys [sum distance]}]]
                     (let []
                       ^{:key (str direction x y)}
                       [:input.flag
                        {:class [(name direction)
                                 (when (and (= direction :down) (zero? sum)) "hide-down")
                                 (when (and (= direction :right) (zero? sum)) "hide-right")
                                 (when (or (zero? x) (= (dec y-shape) y)) "exclude-down")
                                 (when (or (zero? y) (= (dec x-shape) x)) "exclude-right")]
                         :data-direction direction
                         :default-value sum
                         :on-change #(update-sum-fn x y %)}]))))
         (= type :entry)
         [:span.piece-container (:value square)])])

(defn post-request-solution [flags-to-be-solved]
  ;; (spyx "post-request-solution" flags-to-be-solved)
  (do (reset! is-requesting true)
      (POST "http://localhost:3001/solve"
            {:headers {"content-type" "application/edn"}
             :body (str "{:flags-to-be-solved " flags-to-be-solved "}")
             :handler #(let [solution (:solution %)]
                         (do (reset! is-requesting false)
                             (if (empty? solution)
                               (reset! is-no-solution true)
                               (do (reset! is-success true)
                                   (reset-board! (util/board-solution->board-with-solutions @board solution))))))
             :error-handler #(do (reset! is-requesting false)
                                 (reset! is-timeout true)
                                 (.error js/console (str "error: " %)))})))

(defn change-square-type! [x y new-type]
  (do (clear!)
      (swap! board assoc-in [y x] {:type new-type :x x :y y :value nil})))

(defn new-flag [direction]
  {direction {:sum 0 :distance 0}})

(defn change-square-to-flag! [x y]
  (do (clear!)
      (swap! board assoc-in [y x]
             {:type :flag :x x :y y
              :flags {:down {:sum 0 :distance 0}
                      :right {:sum 0 :distance 0}}})))

(defn on-click-square [x y {:keys [type]}]
  (cond (= type :entry)
        (change-square-type! x y :black)
        (= type :black)
        (change-square-type! x y :entry)))

(defn on-double-click-square [x y {:keys [type]}]
  (if (= type :flag)
    (change-square-type! x y :entry)
    (change-square-to-flag! x y)))

(defn update-sum-fn [x y e]
  (let [new-sum (-> e .-target .-value js/parseInt)
        direction (.getAttribute (-> e .-target) "data-direction")]
    (do (clear!)
        (swap! board assoc-in [y x :flags (keyword direction) :sum] new-sum))))

(defn fix-board! [b]
  (let [new-board (-> b util/fix-entries util/fix-flags)]
    (reset-board! new-board)))

(defn main []
  (letfn [(request-solution []
            (do (fix-board! @board)
                (post-request-solution (util/board->flags-to-be-solved @board))))
          (keyboard-listeners [e]
            (let [is-enter (= (.-keyCode e) 13)
                  is-c (= (.-keyCode e) 67)
                  is-f (= (.-keyCode e) 70)
                  is-s (= (.-keyCode e) 83)
                  is-plus (= (.-keyCode e) 187)
                  is-minus (= (.-keyCode e) 189)
                  is-comma (= (.-keyCode e) 188)
                  is-period (= (.-keyCode e) 190)
                  height (count @board)
                  width (-> @board first count)]
              (cond is-c (clear!)
                    is-f (fix-board! @board)
                    (or is-enter is-s) (request-solution)
                    (or is-minus is-comma) (when (and (> width 2) (> height 2))
                                             (do (clear!) (reset-board! (util/decrease-board-size @board))))
                    (or is-plus is-period) (when (and (< width 14) (< height 14))
                                             (do (clear!) (reset-board! (util/increase-board-size @board)))))))]
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
               (let [x-shape (count (first @board))
                     y-shape (count @board)]
                 (fn [x square]
                   ^{:key (str x y)}
                   [square-c x y square on-click-square on-double-click-square update-sum-fn x-shape y-shape]))
               row))
            @board)]]
         [:div.button-container
          [:div.button-indicator
           {:class [(when @is-success "is-success")
                    (when @is-timeout "is-timeout")
                    (when @is-no-solution "is-no-solution")
                    (when @is-requesting "is-requesting")]}
           [:button {:on-click #(when (and (false? @is-success)
                                           (false? @is-timeout)
                                           (false? @is-no-solution)
                                           (false? @is-requesting))
                                  (request-solution))}
            "solve"]]]])})))

(defn mount-app-element []
  (when-let [el (gdom/getElement "app")]
    (reagent/render-component [main] el)))

(mount-app-element)
