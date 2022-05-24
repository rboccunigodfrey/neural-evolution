(ns neural-evolution.data.objects-zones
  (:use [neural-evolution.utils.utils]))

;; ---------------- OBJECTS/ZONES ---------------------

(defn create-object [x y w h]
  {:x x :y y :w w :h h})

(defn create-rand-obj []
  (create-object (rand-nth (range 10 700))
                 (rand-nth (range 10 500))
                 (rand-nth (range 10 60))
                 (rand-nth (range 10 60))))


(defn create-food [id x y e]
  {:id id :x x :y y :energy e})


; collisions

(defn collided-obj? [ind obj]
  (and (< (first (:position ind)) (+ (:x obj) (:w obj)))
       (> (first (:position ind)) (:x obj))
       (< (second (:position ind)) (+ (:y obj) (:h obj)))
       (> (second (:position ind)) (:y obj))))

(defn collided-ind? [i1 i2]
  (and (not (= (:id i1) (:id i2)))
       (= (:position i1) (:position i2))))

(defn collided-any-obj? [ind objects]
  (> (count (filter #(collided-obj? ind %) objects)) 0))

(defn collided-any-ind? [ind population]
  (> (count (filter #(collided-ind? ind %) population)) 0))

(defn collided-borders? [ind]
  (or (<= (first (:position ind)) 5) (>= (first (:position ind)) 795)
      (<= (second (:position ind)) 5) (>= (second (:position ind)) 595)))

(def example-object-vec
  [{:x 280 :y 0 :w 20 :h 90}
   {:x 280 :y 110 :w 20 :h 180}
   {:x 280 :y 310 :w 20 :h 180}
   {:x 280 :y 510 :w 20 :h 90}
   #_{:x 500 :y 200 :w 20 :h 200}
   #_{:x 500 :y 0 :w 20 :h 160}
   #_{:x 500 :y 440 :w 20 :h 160}])

(def red-zones
  [#_{:x 0 :y 0 :w 800 :h 20}
   #_{:x 0 :y 0 :w 20 :h 600}
   #_{:x 780 :y 0 :w 20 :h 600}
   #_{:x 0 :y 580 :w 800 :h 20}])


(def green-zones
  [{:x 0 :y 0 :w 200 :h 600}])

(def food-zones
  [{:x 650 :y 250 :w 50 :h 50}
   {:x 50 :y 50 :w 50 :h 50}
   {:x 50 :y 500 :w 50 :h 50}
   {:x 215 :y 180 :w 40 :h 40}
   {:x 215 :y 420 :w 40 :h 40}])

(defn create-rand-food [id]
  (let [possible-pos
        (rand-nth
          (flatten-btm-lvl
            (mapv
              #(for [x (range (:x %)
                              (+ (:x %) (:w %)))
                     y (range (:y %)
                              (+ (:y %) (:h %)))]
                 [x y]) food-zones)))]
    (create-food id
                 (first possible-pos)
                 (second possible-pos)
                 (rand-nth (range 25 125)))))

(defn filter-redzones [population redzones]
  (filterv #(not (collided-any-obj? % redzones)) population))

(defn filter-greenzones [population greenzones]
  (if (empty? green-zones)
    population
    (filterv #(collided-any-obj? % greenzones) population)))
