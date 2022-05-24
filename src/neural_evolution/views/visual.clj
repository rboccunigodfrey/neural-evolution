(ns neural-evolution.views.visual
  (:require
    [quil.core :as q]
    [quil.middleware :as m])
  (:use [neural-evolution.data.synapses]
        [neural-evolution.data.individuals]
        [neural-evolution.data.children]
        [neural-evolution.data.objects-zones]
        [neural-evolution.data.neurons.sensory]
        [neural-evolution.data.neurons.motor]
        [neural-evolution.evolution.selection]
        [neural-evolution.utils.utils]))

; ---------------- QUIL ---------------------

(defn setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/background 255)
  (let [gen-size 250
        genome-size 32
        objects (flatten example-object-vec)
        max-food 120
        food (vec (for [i (range (int (/ max-food 2)))]
                    (create-rand-food i)))]
    {:population       (gen-population gen-size genome-size objects)
     :objects          objects
     :redzones         red-zones
     :greenzones       green-zones
     :gen-age          0
     :generation       0
     :prev-survivors   0
     :gen-size         gen-size
     :genome-size      genome-size
     :tpg              300
     :selection-method :none
     :mutation-method  :replace
     :pheromones       []
     :food             food
     :max-food         max-food
     :deaths           0
     :test-ind         nil
     :mouse-pressed    false}))

(defn update-ind [ind state]
  (let [population (:population state)
        objects (:objects state)
        pheromones (:pheromones state)
        food (:food state)
        motor-output (calc-motor-output ind population objects pheromones food)
        updated-ind (assoc ind
                      :age (inc (:age ind))
                      :energy (double (- (:energy ind)
                                         (+ 0.2
                                            (* 0.2 (max 0 (- (:energy ind) (:max-energy ind))))
                                            (* (:kill-cooldown ind) 0.05))))
                      :color [(if (:hunter ind) 255 0)
                              0
                              (if (:gatherer ind) 255 0)]
                      :killing-id -1
                      :gathering-id -1
                      :pr false
                      :kill-cooldown (if (> (:kill-cooldown ind) 0) (dec (:kill-cooldown ind)) 0)
                      :gather-cooldown (if (> (:gather-cooldown ind) 0) (dec (:gather-cooldown ind)) 0))]
    (if (or (>= 0 (:energy updated-ind)) (empty? motor-output))
      updated-ind
      (if (> (second motor-output) 0)
        (let [moved-ind (((first motor-output) motor-neuron-functions) updated-ind population food)]
          (if (collided-borders? updated-ind)
            updated-ind
            (if (or (collided-any-obj? moved-ind objects)
                    (collided-any-ind? moved-ind population))
              updated-ind
              moved-ind)))
        updated-ind))))

(defn update-state [state]
  (if (and (< (:gen-age state) (:tpg state)) (not-every? #(<= (:energy %) 0) (:population state)))
    (let [population (:population state)
          death-ids (distinct (filterv #(>= % 0) (mapv :killing-id population)))
          remaining-population (filterv #(not (in? death-ids (:id %))) population)
          eaten-food-ids (distinct (filterv #(>= % 0) (mapv :gathering-id population)))
          remaining-food (filterv
                           #(> (:energy %) 0)
                           (mapv
                             #(if (in? eaten-food-ids (:id %)) (update % :energy - 15) (update % :energy - 0.5))
                             (:food state)))]
      (assoc state
        :population (map #(update-ind % state) remaining-population)
        :pheromones [] #_(concat
                           (mapv #(update % :strength - 2) (filter #(not (<= (:strength %) 0)) (:pheromones state)))
                           (mapv #(hash-map :position (:position %) :strength 30 :id (:id %)) (filter #(:pr %) population)))
        :gen-age (inc (:gen-age state))
        :deaths (+ (:deaths state) (count death-ids))
        :food (if (and (zero? (rand-int 2)) (< (count remaining-food) (:max-food state)))
                (conj
                  remaining-food
                  (create-rand-food (if (zero? (count remaining-food)) 0 (inc (:id (last remaining-food))))))
                remaining-food)
        #_(for [i (range (- (:max-food state) (count remaining-food)))]
            (create-rand-food (inc (+ (inc i) (:id (last remaining-food))))))
        :test-ind (if (and (q/mouse-pressed?) (not (:mouse-pressed state)))
                    (hash-map :id -1
                              :age (:gen-age state)
                              :position [(q/mouse-x) (q/mouse-y)]
                              :energy 100)
                    (if (:test-ind state)
                      (assoc (:test-ind state) :age (:gen-age state))
                      (:test-ind state)))
        :mouse-pressed (q/mouse-pressed?)))
    (let [new-objects (:objects state) #_(concat example-object-vec (repeatedly 10 create-rand-obj))]
      (assoc state
        :population
        (gen-children (:population state)
                      new-objects
                      (:redzones state)
                      (:greenzones state)
                      (:mutation-method state)
                      (:selection-method state)
                      (:gen-size state)
                      (:genome-size state))
        :objects new-objects
        :food (vec (for [i (range (/ (:max-food state) 2))]
                     (create-rand-food i)))
        :gen-age 0
        :deaths 0
        :generation (inc (:generation state))
        :prev-survivors (count
                          (filter-redzones
                            (filter-greenzones
                              ((select-method (:selection-method state))
                               (:population state))
                              (:greenzones state))
                            (:redzones state)))))))

(defn draw-ind [ind]
  (let [size 5
        color (:color ind)
        position (:position ind)]
    (q/fill (first color)
            (second color)
            (last color))
    (q/stroke 255 255 0
              (min 255 (* 3 (:energy ind))))
    (q/ellipse (first position) (second position) size size)))

(defn draw-obj [obj]
  (q/fill 100)
  (q/rect (:x obj) (:y obj) (:w obj) (:h obj)))

(defn draw-pheromone [pheromone]
  (let [size 5
        position (:position pheromone)]
    (q/fill 0 (:strength pheromone))
    (q/ellipse (first position) (second position) size size)))

(defn draw-food [food]
  (let [size (* 2 (Math/log (:energy food)))]
    (q/fill 0 255 0)
    (q/ellipse (:x food) (:y food) size size)))

(defn draw-redzone [redzone]
  (q/fill 255 0 0 50)
  (q/rect (:x redzone) (:y redzone) (:w redzone) (:h redzone)))

(defn draw-greenzone [greenzone]
  (q/fill 0 255 0 50)
  (q/rect (:x greenzone) (:y greenzone) (:w greenzone) (:h greenzone)))

(defn draw-state [state]
  #_(q/background 86 125 70)
  (q/background 50)
  (q/no-stroke)
  (q/fill 255)
  (q/rect 800 0 200 600)
  (doseq [i (range 10)]
    (q/fill 100 (- 10 i))
    (q/rect 800 0 (- 10 i) 600))
  (doseq [redzone (:redzones state)]
    (draw-redzone redzone))
  (doseq [greenzone (:greenzones state)]
    (draw-greenzone greenzone))
  (doseq [obj (:objects state)]
    (draw-obj obj))
  #_(doseq [pheromone (:pheromones state)]
      (draw-pheromone pheromone))
  (doseq [food (:food state)]
    (draw-food food))
  (doseq [ind (:population state)]
    (draw-ind ind))
  (q/no-stroke)
  (q/fill 255 0 0)
  (if (:test-ind state) (q/ellipse (first (:position (:test-ind state)))
                                   (second (:position (:test-ind state)))
                                   10 10)
                        nil)
  (q/fill 0)
  (q/text (str "Generation: " (:generation state)
               "\nGen time: " (:gen-age state)
               "\nPrevious survivors: " (:prev-survivors state)
               #_"\nPheromone count: " #_(count (:pheromones state))
               "\nMost kills: " (:kill-count (apply max-key :kill-count (:population state)))
               "\nHighest energy: " (int (:energy (apply max-key :energy (:population state))))
               "\nDeaths: " (:deaths state)
               "\nFood count: " (count (:food state))
               "\nHunters: " (count (filter :hunter (:population state)))
               "\nGatherers: " (count (filter :gatherer (:population state))))
          830 20)
  (q/text "Selected location sensory data" 830 280)
  (q/text (str "Position: "
               (first (:position (:test-ind state))) ", "
               (second (:position (:test-ind state)))) 830 300)
  (if (:test-ind state) (doseq [sn (map-indexed vector (keys sensory-neuron-functions))]
                          (q/text (str sn ": " (float ((get sensory-neuron-functions (second sn))
                                                (:test-ind state)
                                                (:population state)
                                                (:objects state)
                                                (:pheromones state)
                                                (:food state)))) 830 (+ 320 (* 20 (first sn)))))
                        nil))

(defn animate-agents []
  (q/sketch
    :title "Neural Evolution"
    :size [1000 600]
    :setup #'setup
    :update #'update-state
    :draw #'draw-state
    :renderer :p3d
    :features [:keep-on-top]
    :middleware [m/fun-mode m/pause-on-error]))