; Adaptation of David Randall Miller's evolutionary program by Rafael Boccuni-Godfrey
; https://www.youtube.com/watch?v=N3tRFayqVtk

(ns quil-workflow.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string :as str]))

; What we need: data
; - structure of an individual: {:genome ["hex string" "..."] :neural-map [...] :position: {:x x :y y} :angle n :age n}

; - :genome: 8-digit hex code that when converted to binary, is separated into meaningful portions:
;   - 1st bit = source type: 0 = sensory, 1 = internal
;   - next 7 bits = source id: unsigned value % # of neurons = specific neuron source type
;   - next bit = sink type: 0 = internal neuron, 1 = motor neuron
;   - next 7 bits = sink id: unsigned value % # of neurons = specific neuron source type
;   - rest 16 bits = weight: (signed 16 bit int) / 9000

; - :neural-map
;   - Neuron types: each type mapped to a function whose inputs are determined by genome
;   - Neuron classes: internal, sensory, motor

; - :angle
;   - Value between 0 and 7 -> can face in 8 directions
;   - up, right, left, down, up-left. up-right, down-left, down-right

; conceptually
; -

; catch cases:
; - if an internal neuron ends up being wired to, but has no output, ignore it


; Setup code


; sigmoid function
(defn sig [x]
  (/ 1 (+ 1 (Math/exp (- (* x 5))))))


; tangent hyperbolic function
(defn tanh [x]
  (- (* 2 (sig (* 2 x))) 1))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

; flatten to bottom level
(defn flatten-btm-lvl
  [x]
  (if (some #(coll? %) x)
    (filterv #(and (sequential? %) (not-any? sequential? %))
             (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x)))
    x))


; dealing with hex genomes

(defn rand-hex
  "Generated random hex string of given character size"
  [size]
  (let [chars (str/split "0123456789abcdef" #"")]
    (apply str (repeatedly size #(rand-nth chars)))))

(defn hex-to-bin
  "Converts hex string to binary string."
  [hex]
  (str/replace hex #"0|1|2|3|4|5|6|7|8|9|a|b|c|d|e|f"
               {"0" "0000"
                "1" "0001"
                "2" "0010"
                "3" "0011"
                "4" "0100"
                "5" "0101"
                "6" "0110"
                "7" "0111"
                "8" "1000"
                "9" "1001"
                "a" "1010"
                "b" "1011"
                "c" "1100"
                "d" "1101"
                "e" "1110"
                "f" "1111"}))

(defn mutate-hex
  "Swaps a random character in a hex string."
  [hex]
  (let [hex (str/split hex #"")
        replacements (str/split "0123456789abcdef" #"")
        hex-size (count hex)
        split-loc (rand-int hex-size)]
    (apply str
           (flatten
             [(take split-loc hex)
              (rand-nth replacements)
              (take-last (- hex-size split-loc) hex)]))))

(defn bin-map-32
  "Converts a 32 bit binary string into a map containing values
  useful for neural net info."
  [bin-str]
  (let [bin (str/split bin-str #"")
        take-drop-str #(take %1 (drop %2 bin))
        weight (take-last 16 bin)
        str-seq-to-int #(Long/parseLong (apply str %) 2)]
    {:source-type (Integer/parseInt (first bin))
     :source-id (str-seq-to-int (take-drop-str 7 1)) ; unsigned conversion
     :sink-type (str-seq-to-int (take-drop-str 1 8))
     :sink-id (str-seq-to-int (take-drop-str 7 9)) ; unsigned conversion
     :weight (int (* (if (= "0" (first weight)) 1 -1)
                     (str-seq-to-int (rest weight))))})) ; signed conversion (signed-magnitude

; Generating neural network

; sensory neuron functions

(defn age [ind pop]
  (:age ind))

(defn bdx [ind pop]
  (let [x (first (:position ind))
        dist-left (Math/abs (- x 0))
        dist-right (Math/abs (- x 800))]
    (min dist-left dist-right)))

(defn bdy [ind pop]
  (let [y (second (:position ind))
        dist-top (Math/abs (- y 0))
        dist-bottom (Math/abs (- y 600))]
    (min dist-top dist-bottom)))

(defn bd [ind pop]
  (min (bdx ind pop) (bdy ind pop)))

; coll

; motor neuron functions

; helper
(defn move-by [ind delta-x delta-y]
  (let [pos-vec (:position ind)
        new-x (+ (first pos-vec) delta-x)
        new-y (+ (second pos-vec) delta-y)]
    (assoc ind :position [new-x new-y])))

(defn move-rand [ind pop]
  (let [rand-move (rand-nth [-1 1])]
    (move-by ind rand-move rand-move)))

(defn move-right [ind pop]
  (move-by ind 1 0))
(defn move-left [ind pop]
  (move-by ind -1 0))
(defn move-up [ind pop]
  (move-by ind 0 -1))
(defn move-down [ind pop]
  (move-by ind 0 1))

(def sensory-neuron-functions
  {:age age
   :bdx bdx
   :bdy bdy
   :bd bd})

; coll
(def motor-neuron-functions
  {:mrnd move-rand
   :mr move-right
   :ml move-left
   :mu move-up
   :md move-down
   })

(def internal-neurons
  {:int1 0.1
   :int2 0.2
   :int3 0.3
   :int4 0.4
   :int5 0.5
   :int6 0.6
   :int7 0.7
   :int8 0.8
   :int9 0.9
   :int10 1.0
   })


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

(defn gen-synapse-vec [genome]
  (mapv gen-synapse-map
        genome))


(def example-syn-vec [{:sink-neuron :int5, :weight -1.98425, :source-neuron :bd}
                      {:sink-neuron :int10, :weight 2.3115, :source-neuron :int5}
                      {:sink-neuron :int2, :weight 2.122625, :source-neuron :int10}
                      {:sink-neuron :ml, :weight 0.56625, :source-neuron :int2}
                      {:sink-neuron :int2, :weight 0.725, :source-neuron :int1}
                      {:sink-neuron :int7, :weight 0.8656, :source-neuron :int2}
                      {:sink-neuron :mu, :weight 0.9345, :source-neuron :int7}])


(defn filter-dup-synapses [syn-map]
  (mapv first (vals (group-by
                      #(vector (first (vals %)) (last (vals %)))
                      (filter #(not (= (:source-neuron %) (:sink-neuron %))) syn-map)))))


(defn get-weighted-paths
  [ind pop]
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
                                                       ((get sensory-neuron-functions %) ind pop)
                                                       (get internal-neurons %)))
                                        source-neurons))]

    (apply merge-with concat
           (mapv (fn [mot-syn]
                   (letfn [(populate-values
                             [motor-input-tree cur-syn]
                             (conj motor-input-tree
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
                               (reverse (populate-values motor-input-tree cur-syn))))]
                     (hash-map
                       (:sink-neuron mot-syn)
                       (recur-syn [] mot-syn 0))))
                 mot-sink-syn-vec))))

(defn calc-motor-output [ind pop]
  (let [mot-val-map
        (into {} (for [[k v] (get-weighted-paths ind pop)]
          [k (tanh (apply
                     * (map (fn [syn-seq]
                              (reduce #(tanh (apply * %1 %2))
                                      (tanh (apply * (first (partition 2 syn-seq))))
                                      (rest (partition 2 syn-seq))))
                            (flatten-btm-lvl (vector v)))))]))]
    (if (empty? mot-val-map)
      []
      (apply max-key val mot-val-map))))


(defn new-individual [genome-size]
  (let [genome (map str (repeatedly genome-size #(rand-hex 8)))]
    {:genome genome
     :neural-map (filter-dup-synapses (gen-synapse-vec genome))
     :position [(rand-int 800) (rand-int 600)]
     :age 0}))

; Quil code

(defn setup []
  (q/smooth)
  (q/frame-rate 30)
  (q/background 255)
  {:population (vec (repeatedly 500 #(new-individual 20)))
   :gen-age 0
   :gen-size 500})

(defn update-ind [ind pop]
  (let [motor-output (calc-motor-output ind pop)]
    (update (if (empty? motor-output)
              ind
              (if (> (second motor-output) 0)
                (((first motor-output) motor-neuron-functions) ind pop)
                ind))
            :age inc)))

(defn update-state [state]
    (assoc state
      :population (map #(update-ind % (:population state)) (:population state))))

(defn draw-ind [ind]
  (let [size 5
        position (:position ind)
        age (:age ind)]
    (q/fill 0)
    (q/ellipse (first position) (second position) size size)))

(defn draw-state [state]
  (q/background 255)
  (q/no-stroke)
  (doseq [ind (:population state)]
    (draw-ind ind)))

(defn create-sketch []
  (q/sketch
    :title "Neural Evolution"
    :size [800 600]
    :setup #'setup
    :update #'update-state
    :draw #'draw-state
    :renderer :p3d
    :features [:keep-on-top]
    :middleware [m/fun-mode m/pause-on-error]))




;; sigmoid function: placed as the last layer of a machine learning model
;; can serve to convert the model's output into a probability score
;; Also give values between "0 & 1"

;; tanh function: ensures that the values stay between "-1 & 1",
;; thus regulating the output of the neural network