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







(defn neural-paths [ind population objects pheromones food]
  (let [syn-vec (filter-dup-synapses (:neural-map ind))
        get-parents (fn [child]
                      (filterv #(= (:sink-neuron %)
                                   (:source-neuron child))
                               syn-vec))
        get-children (fn [parent]
                       (filterv #(= (:sink-neuron parent)
                                    (:source-neuron %))
                                syn-vec))
        get-value (fn [syn]
                    (if (contains? sensory-neuron-functions (:source-neuron syn))
                      (((:source-neuron syn) sensory-neuron-functions) ind population objects pheromones food)
                      ((:source-neuron syn) internal-neurons)))
        origin-syn-vec (filterv #(empty? (get-parents %)) syn-vec)
        add-to-path (fn [path syn-vec]
                      (conj
                        path
                        (if (in? origin-syn-vec syn-vec)
                          [(:source-neuron syn-vec) (:weight syn-vec) (get-value syn-vec) (:sink-neuron syn-vec)]
                          [(:source-neuron syn-vec) (:weight syn-vec) (:sink-neuron syn-vec)])))]
    (println (apply str (interleave (sort-by :sink-neuron syn-vec) (repeat "\r\n"))))
    (mapv
      (fn [origin-syn]
        (letfn [(recur-syn [motor-outputs cur-syn depth]
                  (if (> depth 50)
                    (conj motor-outputs "error")
                    (if (contains? motor-neuron-functions (:sink-neuron cur-syn))
                      (add-to-path motor-outputs cur-syn)
                      (mapv
                        #(recur-syn
                           (conj
                             motor-outputs
                             (add-to-path motor-outputs cur-syn)
                             #_(if (in? origin-syn-vec %)
                                 [(:source-neuron cur-syn) (get-value cur-syn) (:weight cur-syn)]
                                 [(:source-neuron cur-syn) (:weight cur-syn)]))
                           %
                           (inc depth))
                        (get-children cur-syn)))))]
          (recur-syn [] origin-syn 0))) origin-syn-vec)))

(defn collate-values [ind population objects pheromones food]
  (let [neural-vals (distinct
                      (filter
                        not-empty
                        (flatten-btm-lvl (neural-paths ind population objects pheromones food))))]
    (loop [motor-vals {} known-vals {} remaining neural-vals]
      (if (= (count (filter #(contains? motor-neuron-functions
                                        (:sink-neuron %)) (:neural-map ind))) (count motor-vals))
        motor-vals
        (if (contains? motor-neuron-functions (last (first remaining)))
          (if (= 4 (count (first remaining)))
            (recur (assoc motor-vals
                     (last (first remaining))
                     (if (contains? motor-vals (last (first remaining)))
                       (tanh (+ (tanh (apply * (take 2 (rest (first remaining)))))
                                ((last (first remaining)) motor-vals)))
                       (tanh (apply * (take 2 (rest (first remaining)))))))
                   known-vals
                   (rest remaining))
            (if (every? #(contains? known-vals (first %))
                        (filter #(= (first (first remaining))
                                    (last %)) neural-vals))
              (recur (assoc motor-vals
                       (last (first remaining))
                       (reduce
                         #(tanh (+ %1 %2))
                         (mapv #(* ((first %) known-vals)
                                   (second (first remaining)))
                               (filter #(= (first (first remaining))
                                           (last %)) neural-vals))))
                     known-vals
                     (rest remaining))
              (recur motor-vals known-vals (conj (rest remaining) (first remaining)))))
          (if (= 4 (count (first remaining)))
            (recur motor-vals
                   (assoc known-vals
                     (first (first remaining))
                     (if (contains? known-vals (first (first remaining)))
                       (tanh (+ (tanh (apply * (take 2 (rest (first remaining)))))
                                ((first (first remaining)) known-vals)))
                       (tanh (apply * (take 2 (rest (first remaining)))))))
                   (rest remaining))
            (if (every? #(contains? known-vals (first %))
                        (filter #(= (first (first remaining))
                                    (last %)) neural-vals))
              (recur motor-vals
                     (assoc known-vals
                       (first (first remaining))
                       (reduce
                         #(tanh (+ %1 %2))
                         (mapv #(* ((first %) known-vals)
                                   (second (first remaining)))
                               (filter #(= (first (first remaining))
                                           (last %)) neural-vals))))
                     (rest remaining))
              (recur motor-vals known-vals (conj (rest remaining) (first remaining))))))))))
