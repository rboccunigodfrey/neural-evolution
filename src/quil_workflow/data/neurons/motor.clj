(ns quil-workflow.data.neurons.motor)

; motor neuron functions

(defn move-angle [ind dir]
  (let [pos-vec (:position ind)
        angle (* dir 45)
        dist-x (* 5 (Math/round (Math/cos (* angle (/ Math/PI 180)))))
        dist-y (* 5 (Math/round (Math/sin (* angle (/ Math/PI 180)))))]
    (assoc ind :position [(+ (first pos-vec) dist-x) (+ (second pos-vec) dist-y)]
               :direction dir)))

(defn release-pheromone [ind _]
  (assoc ind :pr (not (:pr ind))))

(def motor-neuron-functions
  {:mrnd (fn [ind _] (move-angle ind (rand-int 8)))
   :mr   (fn [ind _] (move-angle ind 0))
   :mdr  (fn [ind _] (move-angle ind 1))
   :md   (fn [ind _] (move-angle ind 2))
   :mdl  (fn [ind _] (move-angle ind 3))
   :ml   (fn [ind _] (move-angle ind 4))
   :mul  (fn [ind _] (move-angle ind 5))
   :mu   (fn [ind _] (move-angle ind 6))
   :mur  (fn [ind _] (move-angle ind 7))
   #_:rlp  #_release-pheromone})