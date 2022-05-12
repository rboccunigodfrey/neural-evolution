(ns neural-evolution.utils.utils
  (:require
    [clojure.core.async :refer [>! alts!! timeout chan go]]))

; sigmoid function
(defn sig [x]
  (/ 1 (+ 1 (Math/exp (- (* x 5))))))

; tangent hyperbolic function
(defn tanh [x]
  (- (* 2 (sig (* 2 x))) 1))

; check if val in coll
(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

; flatten to bottom level
(defn flatten-btm-lvl
  [x]
  (if (some #(coll? %) x)
    (filterv #(and (sequential? %) (not-any? sequential? %))
             (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x)))
    x))

; distance formula
(defn distance-general [x1 x2 y1 y2]
  (Math/sqrt (+ (* (- x2 x1)
                   (- x2 x1))
                (* (- y2 y1)
                   (- y2 y1)))))

; distance of two individuals

(defn distance [i1 i2]
  (let [x1 (first (:position i1))
        x2 (first (:position i2))
        y1 (second (:position i1))
        y2 (second (:position i2))]
    (distance-general x1 x2 y1 y2)))

(defn distance-food [i1 food]
  (let [x1 (first (:position i1))
        x2 (:x food)
        y1 (second (:position i1))
        y2 (:y food)]
    (distance-general x1 x2 y1 y2)))

; distance from individual to closest point on object
(defn distance-obj [ind obj]
  (let [rect-min-x (:x obj)
        rect-min-y (:y obj)
        rect-max-x (+ (:x obj) (:w obj))
        rect-max-y (+ (:y obj) (:h obj))
        ind-x (first (:position ind))
        ind-y (second (:position ind))
        dx (Math/max (- rect-min-x ind-x) (Math/max 0 (- ind-x rect-max-x)))
        dy (Math/max (- rect-min-y ind-y) (Math/max 0 (- ind-y rect-max-y)))]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn wait [ms f & args]
  (let [c (chan)]
    (go (>! c (apply f args)))
    (first (alts!! [c (timeout ms)]))))