(ns neural-evolution.data.synapses
  (:use [neural-evolution.data.neurons.internal]
        [neural-evolution.data.neurons.sensory]
        [neural-evolution.data.neurons.motor]
        [neural-evolution.data.genomes]
        [neural-evolution.utils.utils]))

; Generating neural network

(defn gen-synapse-map [gene]
  (let [bin-map (bin-map-32 (hex-to-bin gene))]
    (hash-map
      :source-neuron
      (if
        (= 0 (:source-type bin-map))
        (nth (keys sensory-neuron-functions)
             (mod (:source-id bin-map) (count sensory-neuron-functions)))
        (nth (keys internal-neurons)
             (mod (:source-id bin-map) (count internal-neurons))))
      :sink-neuron
      (if
        (= 0 (:sink-type bin-map))
        (nth (keys motor-neuron-functions)
             (mod (:sink-id bin-map) (count motor-neuron-functions)))
        (nth (keys internal-neurons)
             (mod (:sink-id bin-map) (count internal-neurons))))
      :weight (double (/ (:weight bin-map) 8000)))))

(defn filter-dup-synapses [syn-vec]
  (mapv first (vals (group-by
                      #(vector (first (vals %)) (last (vals %)))
                      (filter #(not (= (:source-neuron %) (:sink-neuron %))) syn-vec)))))

(defn gen-synapse-vec [genome]
  (filter-dup-synapses
    (mapv gen-synapse-map
          genome)))

#_(def example-syn-vec
    [{:sink-neuron :int5, :weight -1.98425, :source-neuron :bd}
     {:sink-neuron :int10, :weight 2.3115, :source-neuron :int5}
     {:sink-neuron :int2, :weight 2.122625, :source-neuron :int10}
     {:sink-neuron :ml, :weight 0.56625, :source-neuron :int2}
     {:sink-neuron :int2, :weight 0.725, :source-neuron :int1}
     {:sink-neuron :int7, :weight 0.8656, :source-neuron :int2}
     {:sink-neuron :mu, :weight 0.9345, :source-neuron :int7}])

(defn get-weighted-paths
  [ind population objects pheromones food]
  (let [syn-vec (:neural-map ind)
        source-neurons (distinct (map #(get % :source-neuron) syn-vec))
        sink-neurons (distinct (map #(get % :sink-neuron) syn-vec))
        int-sink-neurons (filter #(contains? internal-neurons %) sink-neurons)
        int-sink-syn-vec (filter #(contains? internal-neurons
                                             (:sink-neuron %)) syn-vec)
        mot-sink-syn-vec (filter #(contains? motor-neuron-functions
                                             (:sink-neuron %)) syn-vec)
        source-values (apply merge (map #(hash-map % (if
                                                       (contains? sensory-neuron-functions %)
                                                       ((get sensory-neuron-functions %) ind population objects pheromones food)
                                                       (get internal-neurons %)))
                                        source-neurons))]
    (apply merge-with concat
           (mapv (fn [mot-syn]
                   (letfn [(populate-values
                             [motor-input-tree cur-syn]
                             (concat (vec motor-input-tree)
                                     (vector ((:source-neuron cur-syn) source-values)
                                             (:weight cur-syn))))
                           (recur-syn
                             [motor-input-tree
                              cur-syn
                              recur-depth]
                             (if
                               (and (in? int-sink-neurons (:source-neuron cur-syn))
                                    (< 300 recur-depth))
                               (mapv #(recur-syn
                                        (populate-values motor-input-tree cur-syn)
                                        % (inc recur-depth))
                                     (filter #(= (:sink-neuron %)
                                                 (:source-neuron cur-syn))
                                             int-sink-syn-vec))
                               (vec (populate-values motor-input-tree cur-syn))))]
                     (hash-map
                       (:sink-neuron mot-syn)
                       (vec (recur-syn [] mot-syn 0)))))
                 mot-sink-syn-vec))))

(defn calc-motor-output [ind population objects pheromones food]
  (let [mot-val-map
        (into {} (for [[k v] (get-weighted-paths ind population objects pheromones food)]
                   [k (tanh (apply
                              * (map (fn [syn-seq]
                                       (reduce #(tanh (apply * %1 %2))
                                               (tanh (apply * (first (partition 2 syn-seq))))
                                               (rest (partition 2 syn-seq))))
                                     (flatten-btm-lvl (vector v)))))]))]
    (if (empty? mot-val-map)
      []
      (apply max-key val mot-val-map))))
