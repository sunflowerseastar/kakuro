(ns ^:figwheel-hooks cross-sums.core
  (:require
   [goog.dom :as gdom]
   [cross-sums.helpers :refer [get-options]]
   [reagent.core :as reagent :refer [atom create-class]]))

(defn generate-board []
  [[{:type :black}
    {:type :summands :down 4 :down-options (get-options 4 2)}
    {:type :summands :down 6 :down-options (get-options 6 2)}]
   [{:type :summands :right 3 :right-options (get-options 3 2)} {:type :entry} {:type :entry}]
   [{:type :summands :right 7 :right-options (get-options 7 2)} {:type :entry} {:type :entry}]])

(def board (atom (generate-board)))

(defn get-app-element []
  (gdom/getElement "app"))

(defn get-square [b x y] (get (get b y) x))

(defn get-right-summands [b x y]
  (loop [x x y y]
    (let [sq (get-square b (dec x) y)]
      (if (= (:type sq) :summands) sq
          (recur (dec x) y)))))

(defn get-down-summands [b x y]
  (loop [x x y y]
    (let [sq (get-square b x (dec y))]
      (if (= (:type sq) :summands) sq
          (recur x (dec y))))))

(defn get-right-summands-2 [b x y]
  (loop [x x y y n 0]
    (let [sq (get-square b (dec x) y)]
      (if (= (:type sq) :summands)
        ;; (update-in sq [:right-options] #(map (fn [x] (get x 1)) %))
        (set (map #(get % n) (:right-options sq)))
        (recur (dec x) y (inc n))))))

(defn get-down-summands-2 [b x y]
  (loop [x x y y n 0]
    (let [sq (get-square b x (dec y))]
      (if (= (:type sq) :summands)
        (set (map #(get % n) (:down-options sq)))
        (recur x (dec y) (inc n))))))

(defn x-distance-from-summand [b x y]
  (loop [x x y y n 0]
    (let [sq (get-square b (dec x) y)]
      (if (= (:type sq) :summands) n
          (recur (dec x) y (inc n))))))

(defn y-distance-from-summand [b x y]
  (loop [x x y y n 0]
    (let [sq (get-square b x (dec y))]
      (if (= (:type sq) :summands) n
          (recur x (dec y) (inc n))))))

(defn fb [b]
  (let [x-len (count b)
        y-len (count (get b 0))]
    (loop [x 1 y 1 b b]
      ;; (println "x" x "y" y b)
      (let [sq (get-square b x y)
            new-x (if (>= (inc x) x-len) 0 (inc x))
            new-y (if (>= (inc x) x-len) (inc y) y)]
        (cond (or (>= x x-len) (>= y y-len)) b
              (= (:type sq) :entry)
              (let [down-options (:down-options (get-down-summands @board x y))
                    right-options (:right-options (get-right-summands @board x y))
                    down-options-2 (get-down-summands-2 @board x y)
                    right-options-2 (get-right-summands-2 @board x y)
                    ints (clojure.set/intersection down-options-2 right-options-2)
                    x-distance (x-distance-from-summand @board x y)
                    y-distance (y-distance-from-summand @board x y)]
                (cond (and (zero? x-distance) (zero? y-distance))
                      (do (println "entry" x y right-options (first ints))
                          (recur new-x new-y (assoc-in b [y x :value] (first ints))))
                      (zero? y-distance)
                      (let [prev-val (:value (get-square b (dec x) y))
                            val-that-matches-from-right-options (get (first (filter #(= (first %) prev-val) right-options)) x-distance)]
                        (do (println "entry x-only")
                            (recur new-x new-y (assoc-in b [y x :value] val-that-matches-from-right-options))))
                      (zero? x-distance)
                      (let [prev-val (:value (get-square b x (dec y)))
                            val-that-matches-from-down-options (get (first (filter #(= (first %) prev-val) down-options)) y-distance)]
                        (do (println "entry y-only" prev-val down-options y-distance)
                            (recur new-x new-y (assoc-in b [y x :value] val-that-matches-from-down-options))))
                      :else (let [prev-x (:value (get-square b (dec x) y))
                                  prev-y (:value (get-square b x (dec y)))
                                  right-matches (set (map second (filter #(= (first %) prev-x) right-options)))
                                  down-matches (set (map second (filter #(= (first %) prev-y) down-options)))
                                  ints-2 (clojure.set/intersection right-matches down-matches)]
                              (do (println "else" right-options right-matches down-options down-matches (first ints-2))
                                  (recur new-x new-y (assoc-in b [y x :value] (first ints-2)))))))
              :else (do (println "type" (:type sq))
                        (recur new-x new-y b)))))))

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
                             (fn [x square] ^{:key (str x y)}
                               [square-c x y square])
                             row))
                          (fb @board))]]])}))

(defn mount [el]
  (reagent/render-component [main] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
