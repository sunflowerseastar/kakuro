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
(def current-board-index (atom 0))
(def is-board-modified (atom false))
(def board (atom (util/flags-to-be-solved->board (nth boards/boards @current-board-index))))

;; "ui"
(def is-requesting (atom false))
(def is-timeout (atom false))
(def is-no-solution (atom false))
(def is-success (atom false))

;; :clue | :normal
(def click-mode (atom :normal))

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
  (do (reset! is-board-modified true)
      (clear-ui!)
      (clear-board!)))

(defn previous-or-next-board! [dec-or-inc]
  (let [new-board-index (mod (dec-or-inc @current-board-index) (count boards/boards))]
    (do (clear-ui!)
        (clear-board!)
        (reset! current-board-index new-board-index)
        (reset-board! (-> (nth boards/boards new-board-index)
                          (util/flags-to-be-solved->board)))
        (reset! is-board-modified false))))

(defn grid-to-font-size [grid]
  (cond
    (> grid 13) 9
    (> grid 11) 10
    (> grid 10) 11
    (> grid 9) 12
    (> grid 8) 13
    (> grid 7) 14
    (> grid 6) 15
    (> grid 5) 16
    :else 20))

(defn square-c [x y {:keys [flags type] :as square} click-fn click-mode dbl-click-fn update-sum-fn x-shape y-shape]
  [:div.square
   {:class [type (when (= (dec y-shape) y) "board-edge-bottom")
            (when (= (dec x-shape) x) "board-edge-right")]
    :on-click #(click-fn x y square click-mode)
    :on-double-click #(dbl-click-fn x y square)
    :style {:grid-column (+ x 1) :grid-row (+ y 1)
            :font-size (str (grid-to-font-size y-shape) "px")}}
   (cond (= type :flag)
         (->> flags
              (map (fn [[direction {:keys [sum distance]}]]
                     (let [down? (= direction :down)
                           right? (= direction :right)]
                       ^{:key (str direction x y)}
                       [:input.flag-input
                        {:class [(name direction)
                                 (when (and down? (or (zero? distance) (zero? sum))) "hide-down")
                                 (when (and down? (or (zero? x) (= (dec y-shape) y))) "exclude-down")
                                 (when (and right? (or (zero? distance) (zero? sum))) "hide-right")
                                 (when (and right? (or (zero? y) (= (dec x-shape) x))) "exclude-right")]
                         :data-direction direction
                         :default-value sum
                         :on-change #(update-sum-fn x y %)}]))))
         (= type :entry)
         [:span.entry-inner (:value square)])])

(defn post-request-solution [flags-to-be-solved]
  ;; (spyx "post-request-solution" flags-to-be-solved)
  (do (reset! is-requesting true)
      (POST "/api/solve"
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
  (let [b @board
        distance-down (util/get-num-entries-below b x y)
        distance-right (util/get-num-entries-right b x y)]
    (do (clear!)
        (swap! board assoc-in [y x]
               {:type :flag :x x :y y
                :flags {:down {:sum 1 :distance distance-down}
                        :right {:sum 1 :distance distance-right}}}))))

(defn on-click-square! [x y {:keys [type]} click-mode]
  (if (= click-mode :clue)
    (if (= type :flag)
      (change-square-type! x y :black)
      (change-square-to-flag! x y))
    (cond (= type :entry)
          (change-square-type! x y :black)
          (and (= type :black) (not= 0 x) (not= 0 y))
          (change-square-type! x y :entry))))

(defn on-dbl-click-square! [x y {:keys [type]}]
  (if (= type :flag)
    (change-square-type! x y :black)
    (change-square-to-flag! x y)))

(defn update-sum-fn [x y e]
  (let [new-sum (-> e .-target .-value js/parseInt)
        direction (.getAttribute (-> e .-target) "data-direction")]
    (do (clear!)
        (swap! board assoc-in [y x :flags (keyword direction) :sum] new-sum))))

(defn fix-board! [b]
  (let [new-board (util/fix-board b)]
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
          [:div.above-board.constrain-width
           [:div.left
            [:a.arrow-left {:on-click #(previous-or-next-board! dec)} "◀"]
            [:a.arrow-right {:on-click #(previous-or-next-board! inc)} "▶"]
            [:span.em {:class (when @is-board-modified "is-dimmed")}
             (str "board " (inc @current-board-index) " of " (count boards/boards))]]
           (let [height (count @board)
                 width (-> @board first count)]
             [:div.right
              [:a.minus {:class (when (not (and (> width 2) (> height 2))) "is-disabled")
                         :on-click #(do (clear!) (reset-board! (util/decrease-board-size @board)))} "–"]
              [:a.plus {:class (when (not (and (< width 14) (< height 14))) "is-disabled")
                        :on-click #(do (clear!) (reset-board! (util/increase-board-size @board)))} "+"]])]
          [:div.board.constrain-width
           {:style {:grid-template-rows (str "repeat(14, " (/ 100 (count @board)) "%)")}}
           (let [x-shape (count (first @board))
                 y-shape (count @board)
                 click-mode @click-mode]
             (map-indexed
              (fn [y row]
                (map-indexed
                 (fn [x square]
                   ^{:key (str x y)}
                   [square-c x y square on-click-square! click-mode on-dbl-click-square! update-sum-fn x-shape y-shape])
                 row))
              @board))]
          [:div.below-board.constrain-width
           [:div.left
            [:a.arrow-left {:on-click #(spyx "left")} "◀"]
            [:a.arrow-right {:on-click #(spyx "right")} "▶"]
            [:span.em "solution 1 of x"]]
           (let [is-clue-mode (= @click-mode :clue)
                 is-normal-mode (= @click-mode :normal)]
             [:div.right
              [:a.mode-selection {:class (when is-clue-mode "is-selected")
                                  :on-click #(when is-normal-mode
                                               (reset! click-mode :clue))}
               [:span "clue"]]
              [:a.mode-selection {:class (when is-normal-mode "is-selected")
                                  :on-click #(when is-clue-mode
                                               (reset! click-mode :normal))}
               [:span "normal"]]])]]
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
