(ns cross-sums.helpers)

(defn get-options [n size]
  (cond (= size 2)
        (for [x (range 0 10) y (range 0 10) :when (and (not= x y) (= (+ x y) n))] [x y])
        (= size 3)
        (for [x (range 0 10) y (range 0 10) z (range 0 10) :when (and (not= x y z) (= (+ x y z) n))] [x y z])))

