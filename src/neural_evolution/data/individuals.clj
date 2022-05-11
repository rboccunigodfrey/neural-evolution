(ns neural-evolution.data.individuals
  (:use [neural-evolution.data.objects-zones]
        [neural-evolution.data.genomes]
        [neural-evolution.data.synapses]))

; population generation

(defn new-individual [genome-size id]
  (let [genome (map str (repeatedly genome-size #(rand-hex 64)))]
    {:id         id
     :genome     genome
     :neural-map (gen-synapse-vec genome)
     :position   [(rand-nth (range 5 800 5))
                  (rand-nth (range 5 600 5))]
     :age        0
     :color      [(rand-int 170) (rand-int 170) (rand-int 170)]
     :pr         false
     :direction  -1
     :pedigree   0}))


(defn gen-population [population-size genome-size objects]
  (loop [population [] ind-success 0]
    (if (= population-size ind-success)
      population
      (let [cand-ind (new-individual genome-size ind-success)]
        (if (or (collided-any-ind? cand-ind population)
                (collided-any-obj? cand-ind objects))
          (recur population ind-success)
          (recur (conj population cand-ind) (inc ind-success)))))))
