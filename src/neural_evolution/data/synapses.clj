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
