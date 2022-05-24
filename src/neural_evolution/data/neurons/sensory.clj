(ns neural-evolution.data.neurons.sensory
  (:use [neural-evolution.utils.utils]))

; constants

(def SMELL_DIST 80)
(def SIGHT_DIST 60)

; sensory neuron functions

(defn age [ind _ _ _ _]
  (:age ind))

(defn energy [ind _ _ _ _]
  (:energy ind))

(defn border-dist-x [ind _ _ _ _]
  (let [x (first (:position ind))
        dist-left (Math/abs ^int (- x 0))
        dist-right (Math/abs ^int (- x 800))]
    (min SIGHT_DIST dist-left dist-right)))

(defn border-dist-y [ind _ _ _ _]
  (let [y (second (:position ind))
        dist-top (Math/abs ^int (- y 0))
        dist-bottom (Math/abs ^int (- y 600))]
    (min SIGHT_DIST dist-top dist-bottom)))

(defn border-dist [ind population objects pheromones food]
  (min (border-dist-x ind population objects pheromones food)
       (border-dist-y ind population objects pheromones food)))

(defn dist-from-center [ind _ _ _ _]
  (min SIGHT_DIST (distance-general (first (:position ind)) (second (:position ind)) 400 300)))

(defn nearest-object-dist [ind _ objects _ _]
  (if (zero? (count objects))
    0
    (min SIGHT_DIST (distance-obj ind (reduce #(if (< (distance-obj ind %1) (distance-obj ind %2)) %1 %2) objects)))))

(defn nearest-neighbour-distance [ind population _ _ _]
  (reduce
    #(if (and (< %1 (distance ind %2))
              (not (= (:id %2) (:id ind)))
              (not (zero? %1)))
       %1 (distance ind %2))
    (distance ind (first population))
    (rest population)))

(defn osc [ind method]
  (let [min-val -4
        max-val 4
        avg-val (/ (+ min-val max-val) 2)
        amp (/ (- max-val min-val) 2)]
    (+ avg-val (* amp (method (* 0.02 (:age ind) Math/PI 2))))))

(defn osc-sin [ind _ _ _ _]
  (osc ind #(Math/sin %)))

(defn osc-cos [ind _ _ _ _]
  (osc ind #(Math/cos %)))


(defn ph-density-radius [ind _ _ pheromones _]
  (let [ind-x (first (:position ind))
        ind-y (second (:position ind))
        dist-ph #(distance-general
                   ind-x
                   (first (:position %))
                   ind-y
                   (second (:position %)))
        near-ph (filter #(and (not (= (:id %) (:id ind)))
                              (< (- (dist-ph %)) 30))
                        pheromones)]
    (* 10 (reduce + (map #((* (:strength %) (/ 1 (+ 1 (dist-ph %))))) near-ph)))
    #_(if (empty? near-ph)
      0
      (* 10 (reduce #(+ %1 (* (:strength %2) (/ 1 (+ 1 (dist-ph %2)))))
                    (* (:strength (first near-ph))
                       (/ 1 (+ 1 (dist-ph (first near-ph)))))
                    (rest near-ph))))))

(defn pop-density-radius [ind population _ _ _]
  (let [pop-near (filter #(and (not (= (:id ind) (:id %)))
                               (< (distance ind %) (/ SMELL_DIST 1.5)))
                         population)]
    (+ (count pop-near) (max 0 (reduce + (map :energy pop-near))))))

(defn food-density-radius [ind _ _ _ food]
  (let [food-near (filter #(< (distance-food ind %) SMELL_DIST)
                         food)]
    (reduce + (map :energy food-near))))

(def sensory-neuron-functions
  {:age  age
   :energy energy
   :bdx  border-dist-x
   :bdy  border-dist-y
   :bd   border-dist
   :oscs osc-sin
   :oscc osc-cos
   :nod  nearest-object-dist
   :pdr  pop-density-radius
   :dfc  dist-from-center
   :fdr  food-density-radius})