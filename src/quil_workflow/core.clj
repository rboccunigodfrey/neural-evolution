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
  "Swaps a random character in a hex string. There's probably a better way I can do this."
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
  useful for neural net info. Probably could do this better too."
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
  (let [x (:x (:position ind))
        dist-left (Math/abs (- x 0))
        dist-right (Math/abs (- x 800))]
    (min dist-left dist-right)))

(defn bdy [ind pop]
  (let [y (:y (:position ind))
        dist-top (Math/abs (- y 0))
        dist-bottom (Math/abs (- y 600))]
    (min dist-top dist-bottom)))

(defn bd [ind pop]
  (min (bdx ind pop) (bdy ind pop)))

; coll

(def sensory-neuron-functions
  {:age age
   :bdx bdx
   :bdy bdy
   :bd bd})

; motor neuron functions,  change by Aryan

; helper
(defn move-by [ind delta-x delta-y]
  (let [pos-vec (:position ind)
        new-x (+ (:x pos-vec) delta-x)
        new-y (+ (:x pos-vec) delta-y)]
    (update ind :position replace [new-x new-y])))

(defn move-rand [ind pop]
  (let [rand-move (rand-nth [-1 1])]
    (move-by ind rand-move rand-move)))

(defn move-towards-nearest [ind pop]
  )

(defn move-right [ind pop]
  (move-by ind 1 0))
(defn move-left [ind pop]
  (move-by ind -1 0))
(defn move-up [ind pop]
  (move-by ind 0 -1))
(defn move-down [ind pop]
  (move-by ind 0 1))

; coll
(def motor-neuron-functions
  {:mrnd move-rand
   :mr move-right
   :ml move-left
   :mu move-up
   :md move-down
   })

(def internal-neurons
  {:int1 0.5
   :int2 1
   :int3 2
   :int4 -0.5
   :int5 -1
   :int6 -2})

(defn gen-synapse-map [ind]
  (let [genome (:genome ind)
        bin-maps (map #(bin-map-32 (hex-to-bin %)) genome)]
    (map #(hash-map
            :source-neuron
            (if
              (= 0 (:source-type %))
              (nth (keys sensory-neuron-functions)
                   (mod (:source-id %) (count sensory-neuron-functions)))
              (nth (keys internal-neurons)
                  (mod (:sink-id %) (count internal-neurons))))
            :sink-neuron
            (if
              (= 0 (:sink-type %))
              (nth (keys motor-neuron-functions)
                   (mod (:sink-id %) (count motor-neuron-functions)))
              (nth (keys internal-neurons)
                   (mod (:sink-id %) (count internal-neurons))))
            :weight (double (/ (:weight %) 8000)))
         bin-maps)))

(defn filter-invalid-synapses [n-map]
  (mapv first (vals (group-by
                     #(vector (first (vals %)) (last (vals %)))
                     (filter #(not (= (:source-neuron %) (:sink-neuron %))) n-map)))))


(def example-syn-vec
  [{:sink-neuron :ml, :weight 1.062375, :source-neuron :int6}
   {:sink-neuron :mu, :weight 3.623125, :source-neuron :age}
   {:sink-neuron :ml, :weight -1.940375, :source-neuron :int3}
   {:sink-neuron :int6, :weight 1.12975, :source-neuron :bdx}
   {:sink-neuron :mu, :weight -1.1455, :source-neuron :bd}
   {:sink-neuron :int1, :weight -2.353625, :source-neuron :bdy}
   {:sink-neuron :md, :weight -1.22175, :source-neuron :int6}
   {:sink-neuron :md, :weight -1.4525, :source-neuron :bdy}
   {:sink-neuron :mr, :weight 3.641875, :source-neuron :bdx}
   {:sink-neuron :mu, :weight 1.219125, :source-neuron :bdx}
   {:sink-neuron :int5, :weight 3.65875, :source-neuron :age}
   {:sink-neuron :mrnd, :weight -0.85775, :source-neuron :bdy}])


(defn apply-neural-map [ind pop]
  (let [ind-syn-map (filter-invalid-synapses (gen-synapse-map ind))]))


(defn group-syn-vec [syn-vec]
  (filter #(and (contains? sensory-neuron-functions (:source-neuron %))
                (contains? motor-neuron-functions (:sink-neuron %)))
          syn-vec))

(defn gen-neural-map [ind]
  (let [syn-vec (group-syn-vec (filter-invalid-synapses (gen-synapse-map ind)))
        sensory-functions (distinct (map #(:source-neuron %) syn-vec))
        motor-functions (distinct (map #(:sink-neuron %) syn-vec))]
    sensory-functions
    ))

(defn new-individual [genome-size screen-size]
  {:genome (map str (repeatedly genome-size #(rand-hex 8)))
   :neural-map '()
   :position {:x (rand-int (:x screen-size)) :y (rand-int (:y screen-size))}})

; Quil code

(defn setup []
  (q/smooth)
  (q/frame-rate 30)
  (q/background 255)
  {:population (vec (repeatedly 500 #(new-individual 8 {:x 800 :y 600})))})

(defn update-state [state]
    {:population (:population state)})

(defn draw-ind [position]
  (let [size 5]
    (q/fill 0)
    (q/ellipse (:x position) (:y position) size size)))

(defn draw-state [state]
  (q/background 255)
  (q/no-stroke)
  (doseq [ind (:population state)]
    (draw-ind (:position ind))))

(defn create-sketch []
  (q/sketch
    :title "Quil Example"
    :size [800 600]
    :setup #'setup
    :update #'update-state
    :draw #'draw-state
    :renderer :p3d
    :features [:keep-on-top]
    :middleware [m/fun-mode m/pause-on-error]))
