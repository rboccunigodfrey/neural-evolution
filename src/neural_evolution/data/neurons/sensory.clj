(ns neural-evolution.data.neurons.sensory
  (:use [neural-evolution.utils.utils]))

; sensory neuron functions

(defn age [ind _ _ _]
  (:age ind))

(defn bdx [ind _ _ _]
  (let [x (first (:position ind))
        dist-left (Math/abs ^int (- x 0))
        dist-right (Math/abs ^int (- x 800))]
    (min dist-left dist-right)))

(defn bdy [ind _ _ _]
  (let [y (second (:position ind))
        dist-top (Math/abs ^int (- y 0))
        dist-bottom (Math/abs ^int (- y 600))]
    (min dist-top dist-bottom)))

(defn bd [ind population objects pheromones]
  (min (bdx ind population objects pheromones) (bdy ind population objects pheromones)))

(defn dist-from-center [ind _ _ _]
  (distance-general (first (:position ind)) (second (:position ind)) 400 300))

(defn nearest-object-dist [ind _ objects _]
  (if (zero? (count objects))
    0
    (distance-obj ind (reduce #(if (< (distance-obj ind %1) (distance-obj ind %2)) %1 %2) objects))))

(defn nnd [ind population _ _]
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
    (+ avg-val (* amp (method (* 0.1 (:age ind) Math/PI 2))))))

(defn osc-sin [ind _ _ _]
  (osc ind #(Math/sin %)))

(defn osc-cos [ind _ _ _]
  (osc ind #(Math/cos %)))


(defn phdr [ind _ _ pheromones]
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
    (if (empty? near-ph)
      0
      (* 10 (reduce #(+ %1 (* (:strength %2) (/ 1 (+ 1 (dist-ph %2)))))
                    (* (:strength (first near-ph))
                       (/ 1 (+ 1 (dist-ph (first near-ph)))))
                    (rest near-ph))))))

(defn dir-to-slope [dir]
  )

(defn position-on-path? [x1 x2 y1 y2 dir]
  (= (/ (- x2 x1) (- y2 y1)) (dir-to-slope dir)))

(defn pdr [ind population _ _]
  (let [pop-near (filter #(and (not (= (:id ind) (:id %)))
                               (< (distance ind %) 50))
                         population)]
    (count pop-near)
    #_(if (empty? pop-near)
        0
        (reduce
          #(+ %1 (distance ind %2))
          (distance ind (first pop-near))
          (rest pop-near)))))

(def sensory-neuron-functions
  {:age  age
   :bdx  bdx
   :bdy  bdy
   :bd   bd
   :oscs osc-sin
   :oscc osc-cos
   :nod  nearest-object-dist
   :pdr  pdr
   :dfc  dist-from-center})