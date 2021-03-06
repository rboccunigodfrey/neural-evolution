(ns neural-evolution.data.neurons.motor
  (:use [neural-evolution.utils.utils]))

; motor neuron functions

(defn move-angle [ind dir]
  (let [pos-vec (:position ind)
        angle (* dir 45)
        dist-x (* 5 (Math/round (Math/cos (* angle (/ Math/PI 180)))))
        dist-y (* 5 (Math/round (Math/sin (* angle (/ Math/PI 180)))))]
    (assoc ind :position [(+ (first pos-vec) dist-x) (+ (second pos-vec) dist-y)]
               :direction dir)))

(defn release-pheromone [ind _ _]
  (assoc ind :pr (not (:pr ind))))


(defn kill [ind population _]
  (let [adj-pop (filter #(and (not (= (:id ind) (:id %)))
                              (< (distance ind %) 5))
                        population)
        prey (first adj-pop)]
    (if (or (zero? (count adj-pop))
            (:gatherer ind)
            (>= (:energy ind) (:max-energy ind))
            (and (not (:gatherer prey)) (>= (:energy prey) (:energy ind)))
            (not (zero? (:kill-cooldown ind))))
      ind
      (assoc ind :killing-id (:id prey)
                 :kill-count (inc (:kill-count ind))
                 :energy (+ (:energy ind) (max 0 (* 0.8 (:energy prey))))
                 :hunter (if (not (:hunter ind))
                           (> (:kill-count ind) 5)
                           true)
                 :kill-cooldown 20))))

(defn gather [ind _ food]
  (let [adj-food (filter #(< (distance-food ind %) 5) food)]
    (if (or (zero? (count adj-food))
            (:hunter ind)
            (>= (:energy ind) (:max-energy ind))
            (not (zero? (:gather-cooldown ind))))
      #_(zero? (count adj-food))
      ind
      (assoc ind :gathering-id (:id (first adj-food))
                 :gather-count (inc (:gather-count ind))
                 :energy (+ (:energy ind) (min 15 (:energy (first adj-food))))
                 :gatherer (if (not (:gatherer ind))
                             (> (:gather-count ind) 10)
                             true)
                 :gather-cooldown 10))))

(def motor-neuron-functions
  {:mrnd (fn [ind _ _] (move-angle ind (rand-int 8)))
   :mr   (fn [ind _ _] (move-angle ind 0))
   :mdr  (fn [ind _ _] (move-angle ind 1))
   :md   (fn [ind _ _] (move-angle ind 2))
   :mdl  (fn [ind _ _] (move-angle ind 3))
   :ml   (fn [ind _ _] (move-angle ind 4))
   :mul  (fn [ind _ _] (move-angle ind 5))
   :mu   (fn [ind _ _] (move-angle ind 6))
   :mur  (fn [ind _ _] (move-angle ind 7))
   :kill kill
   :gth  gather
   #_:rlp  #_release-pheromone})