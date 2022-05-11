(ns neural-evolution.evolution.selection
  (:use [neural-evolution.utils.utils]
        [neural-evolution.data.objects-zones]))

; selection methods

(defn select-right [population]
  (filterv #(> (first (:position %)) 600) population))

(defn select-left [population]
  (filterv #(< (first (:position %)) 200) population))

(defn select-center [population]
  (filterv #(and (> (first (:position %)) 300) (< (first (:position %)) 500)) population))

(defn select-circle [population x y]
  (let [radius 100]
    (filterv
      #(< (distance-general
            (first (:position %)) x
            (second (:position %)) y)
          radius)
      population)))

(defn select-central-circle [population]
  (select-circle population 400 300))

(defn select-left-circle [population]
  (select-circle population 200 300))

(defn select-right-circle [population]
  (select-circle population 600 300))

(defn filter-redzones [population redzones]
  (filterv #(not (collided-any-obj? % redzones)) population))

(defn filter-greenzones [population greenzones]
  (filterv #(collided-any-obj? % greenzones) population))

(defn select-method [method]
  (case method
    :right select-right
    :left select-left
    :center select-center
    :central-circle select-central-circle
    :right-circle select-right-circle
    :left-circle select-left-circle))
