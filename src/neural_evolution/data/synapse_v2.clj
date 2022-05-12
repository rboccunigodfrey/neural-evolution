(ns neural-evolution.data.synapse-v2
  (:use [neural-evolution.data.neurons.internal]
        [neural-evolution.data.neurons.sensory]
        [neural-evolution.data.neurons.motor]
        [neural-evolution.data.genomes]
        [neural-evolution.utils.utils]))

(defn gen-synapse-map [gene]
  (let [bin-map (bin-map-32 (hex-to-bin gene))]
    (assoc bin-map
      :source-id
      (if
        (= 0 (:source-type bin-map))
        (mod (:source-id bin-map) (count sensory-neuron-functions))
        (mod (:source-id bin-map) (count internal-neurons)))
      :sink-id
      (if
        (= 0 (:sink-type bin-map))
        (mod (:sink-id bin-map) (count motor-neuron-functions))
        (mod (:sink-id bin-map) (count internal-neurons)))
      )))


(defn filter-dup-synapses [syn-vec]
  (mapv first (vals (group-by
                      #(vector (first (vals %)) (last (vals %)))
                      (filter #(not (= (:source-id %) (:sink-id %))) syn-vec)))))

(defn gen-synapse-vec [genome]
  (filter-dup-synapses
    (mapv gen-synapse-map
          genome)))

#_(defn gen-syn-node-vec [syn-map syn-vec]
  (if (= 0 (:sink-type syn-map))
    (let [num-outputs (count (filterv #(= () ()) syn-vec))
          num-self-inputs (count (filterv #(= (:source-id %) ()) syn-vec))]
      (hash-map
        :remapped-num
        (:source-num syn-map)
        :num-outputs num-outputs
        :num-self-inputs
        :num-inputs-sensory-internal))))

#_(defn make-syn-nodes [syn-vec]
  (mapv #(gen-syn-node-vec % syn-vec) syn-vec))