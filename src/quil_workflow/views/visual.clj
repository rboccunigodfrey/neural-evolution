(ns quil-workflow.views.visual
  (:require
    [quil.core :as q]
    [quil.middleware :as m])
  (:use [quil-workflow.data.synapses]
        [quil-workflow.data.individuals]
        [quil-workflow.data.children]
        [quil-workflow.data.objects-zones]
        [quil-workflow.data.neurons.motor]
        [quil-workflow.evolution.selection]))

; ---------------- QUIL ---------------------

(defn setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/background 255)
  (let [gen-size 250
        objects (vec example-object-vec
                     #_(concat example-object-vec (repeatedly 10 create-rand-obj)))]
    {:population       (gen-population gen-size 32 objects)
     :objects          objects
     :redzones         red-zones
     :greenzones         green-zones
     :gen-age          0
     :generation       0
     :prev-survivors   0
     :gen-size         gen-size
     :tpg              300
     :selection-method :center
     :mutation-method  :replace
     :pheromones       []}))

(defn update-ind [ind state]
  (let [population (:population state)
        objects (:objects state)
        pheromones (:pheromones state)
        motor-output (calc-motor-output ind population objects pheromones)]
    (assoc (if (empty? motor-output)
             ind
             (if (> (second motor-output) 0)
               (let [moved-ind (((first motor-output) motor-neuron-functions) ind population)]
                 (if (collided-borders? ind)
                   ind
                   (if (or (collided-any-obj? moved-ind objects)
                           (collided-any-ind? moved-ind population))
                     ind
                     moved-ind)))
               ind))
      :age (inc (:age ind)))))

(defn update-state [state]
  (if (< (:gen-age state) (:tpg state))
    (assoc state
      :population (map #(update-ind % state) (:population state))
      :pheromones (concat
                    (mapv #(update % :strength - 2) (filter #(not (<= (:strength %) 0)) (:pheromones state)))
                    (mapv #(hash-map :position (:position %) :strength 30 :id (:id %)) (filter #(:pr %) (:population state))))
      :gen-age (inc (:gen-age state)))
    (let [new-objects (:objects state) #_(concat example-object-vec (repeatedly 10 create-rand-obj))]
      (assoc state
        :population
        (gen-children (:population state)
                      new-objects
                      (:redzones state)
                      (:greenzones state)
                      (:mutation-method state)
                      (:gen-size state))
        :objects new-objects
        :gen-age 0
        :generation (inc (:generation state))
        :prev-survivors (count (filter-redzones (filter-greenzones
                                                  (:population state) (:greenzones state)) (:redzones state)))))))

(defn draw-ind [ind]
  (let [size 5
        position (:position ind)]
    (q/fill (:color ind))
    (q/ellipse (first position) (second position) size size)))

(defn draw-obj [obj]
  (q/fill 100)
  (q/rect (:x obj) (:y obj) (:w obj) (:h obj)))

(defn draw-pheromone [pheromone]
  (let [size 5
        position (:position pheromone)]
    (q/fill 0 (:strength pheromone))
    (q/ellipse (first position) (second position) size size)))

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
  (doseq [pheromone (:pheromones state)]
    (draw-pheromone pheromone))
  (doseq [ind (:population state)]
    (draw-ind ind))
  (q/fill 0)
  (q/text (str "Generation: " (:generation state)
               "\nPrevious survivors: " (:prev-survivors state)
               "\nPheromone count: " (count (:pheromones state))
               #_"\nBest lineage age: " #_(:pedigree (apply max-key :pedigree (:population state))))
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