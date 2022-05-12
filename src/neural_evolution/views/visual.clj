(ns neural-evolution.views.visual
  (:require
    [quil.core :as q]
    [quil.middleware :as m])
  (:use [neural-evolution.data.synapses]
        [neural-evolution.data.individuals]
        [neural-evolution.data.children]
        [neural-evolution.data.objects-zones]
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
        max-food 50
        food (for [i (range max-food)]
               (create-rand-food i))]
    {:population       (gen-population gen-size genome-size objects)
     :objects          objects
     :redzones         red-zones
     :greenzones       green-zones
     :gen-age          0
     :generation       0
     :prev-survivors   0
     :gen-size         gen-size
     :genome-size      genome-size
     :tpg              200
     :selection-method :pos-energy
     :mutation-method  :replace
     :pheromones       []
     :food             food
     :max-food         max-food
     :deaths           0}))

(defn update-ind [ind state]
  (let [population (:population state)
        objects (:objects state)
        pheromones (:pheromones state)
        food (:food state)
        motor-output (calc-motor-output ind population objects pheromones food)
        updated-ind (assoc ind
                      :age (inc (:age ind))
                      :energy (int (- (:energy ind) (+ 1 (/ (* (inc (:kill-count ind))) (inc (:gather-count ind))))))
                      :color [(if (:hunter ind) 255 0)
                              0
                              (if (:gatherer ind) 255 0)]
                      :killing-id -1
                      :gathering-id -1
                      :pr false
                      :kill-cooldown (if (> (:kill-cooldown ind)  0) (dec (:kill-cooldown ind)) 0))]
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
  (if (< (:gen-age state) (:tpg state))
    (let [population (:population state)
          death-ids (distinct (filter #(>= % 0) (mapv :killing-id population)))
          remaining-population (filter #(not (in? death-ids (:id %))) population)
          eaten-food-ids (distinct (filter #(>= % 0) (mapv :gathering-id population)))
          remaining-food (filter #(not (in? eaten-food-ids (:id %))) (:food state))]
      (assoc state
        :population (map #(update-ind % state) remaining-population)
        :pheromones [] #_(concat
                           (mapv #(update % :strength - 2) (filter #(not (<= (:strength %) 0)) (:pheromones state)))
                           (mapv #(hash-map :position (:position %) :strength 30 :id (:id %)) (filter #(:pr %) population)))
        :gen-age (inc (:gen-age state))
        :deaths (+ (:deaths state) (count death-ids))
        :food #_remaining-food
                (if (and (zero? (rand-int 5)) (< (count remaining-food) (:max-food state)))
                  (conj
                    remaining-food
                    (create-rand-food (if (zero? (count remaining-food)) 0 (inc (:id (last remaining-food))))))
                  remaining-food)
                  #_(for [i (range (- (:max-food state) (count remaining-food)))]
                    (create-rand-food (inc (+ (inc i) (:id (last remaining-food))))))))
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
        :food (for [i (range (:max-food state))]
                (create-rand-food i))
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
    (q/ellipse (first position) (second position) size (* 1.5 size))))

(defn draw-obj [obj]
  (q/fill 100)
  (q/rect (:x obj) (:y obj) (:w obj) (:h obj)))

(defn draw-pheromone [pheromone]
  (let [size 5
        position (:position pheromone)]
    (q/fill 0 (:strength pheromone))
    (q/ellipse (first position) (second position) size size)))

(defn draw-food [food]
  (let [size (/ (:energy food) 2)]
    (q/fill 0 255 0)
    (q/ellipse (:x food) (:y food) size size)))

(defn draw-redzone [redzone]
  (q/fill 255 0 0 50)
  (q/rect (:x redzone) (:y redzone) (:w redzone) (:h redzone)))

(defn draw-greenzone [greenzone]
  (q/fill 0 255 0 50)
  (q/rect (:x greenzone) (:y greenzone) (:w greenzone) (:h greenzone)))

(defn draw-state [state]
  (q/background 255)
  (q/no-stroke)
  (q/fill 225)
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
  (q/fill 0)
  (q/text (str "Generation: " (:generation state)
               "\nGen time: " (:gen-age state)
               "\nPrevious survivors: " (:prev-survivors state)
               #_"\nPheromone count: " #_(count (:pheromones state))
               "\nMost kills: " (:kill-count (apply max-key :kill-count (:population state)))
               "\nHighest energy: " (:energy (apply max-key :energy (:population state)))
               "\nDeaths: " (:deaths state)
               "\nFood count: " (count (:food state)))
          830 20))

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