(ns neural-evolution.views.textual
  (:use [neural-evolution.data.synapses]
        [neural-evolution.data.individuals]
        [neural-evolution.data.children]
        [neural-evolution.data.objects-zones]
        [neural-evolution.data.neurons.motor]
        [neural-evolution.evolution.selection]))

; main textual evo loop

(defn evolve-agents
  [gen-size max-gen genome-size tpg s-method m-method]
  (let [init-pop (gen-population gen-size genome-size example-object-vec)]
    (loop [ticks 0
           generation 0
           population init-pop]
      (println (str "Generation: " generation ", Survivors: " (count ((select-method s-method) population))))
      (if (< generation max-gen)
        (if (< ticks tpg)
          (recur (inc ticks) generation
                 (mapv
                   #(let [motor-output (calc-motor-output % population [] [])]
                      (update
                        (if (empty? motor-output)
                          %
                          (if (> (second motor-output) 0)
                            (((first motor-output) motor-neuron-functions) % population)
                            %))
                        :age inc))
                   population))
          (recur 0
                 (inc generation)
                 (gen-children population example-object-vec red-zones green-zones m-method gen-size)))
        (first population)))))
