; Adaptation of David Randall Miller's evolutionary program by Rafael Boccuni-Godfrey and Aryan Raval
; https://www.youtube.com/watch?v=N3tRFayqVtk

(ns quil-workflow.core
  (:require
    [clojure.string :as str]
    [quil.core :as q]
    [quil.middleware :as m]
    [clojure.core.async :refer [>! alts!! timeout chan go]]))

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

(defn wait [ms f & args]
  (let [c (chan)]
    (go (>! c (apply f args)))
    (first (alts!! [c (timeout ms)]))))

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
     :source-id   (str-seq-to-int (take-drop-str 7 1))      ; unsigned conversion
     :sink-type   (str-seq-to-int (take-drop-str 1 8))
     :sink-id     (str-seq-to-int (take-drop-str 7 9))      ; unsigned conversion
     :weight      (int (* (if (= "0" (first weight)) 1 -1)
                          (str-seq-to-int (rest weight))))})) ; signed conversion (signed-magnitude)

; Generating neural network

; sensory neuron functions

(defn distance-general [x1 x2 y1 y2]
  (Math/sqrt (+ (* (- x2 x1)
                   (- x2 x1))
                (* (- y2 y1)
                   (- y2 y1)))))

(defn distance [i1 i2]
  (let [x1 (first (:position i1))
        x2 (first (:position i2))
        y1 (second (:position i1))
        y2 (second (:position i2))]
    (distance-general x1 x2 y1 y2)))


(defn age [ind _ _ _]
  (:age ind))

(defn bdx [ind _ _ _]
  (let [x (first (:position ind))
        dist-left (Math/abs ^int (- x 0))
        dist-right (Math/abs ^int (- x 800))]
    (min dist-left dist-right)))

(defn bdy [ind _ _ _]
  (let [y (second (:position ind))
        dist-top (Math/abs ^int (- y 0))
        dist-bottom (Math/abs ^int (- y 600))]
    (min dist-top dist-bottom)))

(defn bd [ind population objects pheromones]
  (min (bdx ind population objects pheromones) (bdy ind population objects pheromones)))

(defn dist-from-center [ind _ _ _]
  (distance-general (first (:position ind)) (second (:position ind)) 400 300))

(defn distance-obj [ind obj]
  (let [rect-min-x (:x obj)
        rect-min-y (:y obj)
        rect-max-x (+ (:x obj) (:w obj))
        rect-max-y (+ (:y obj) (:h obj))
        ind-x (first (:position ind))
        ind-y (second (:position ind))
        dx (Math/max (- rect-min-x ind-x) (Math/max 0 (- ind-x rect-max-x)))
        dy (Math/max (- rect-min-y ind-y) (Math/max 0 (- ind-y rect-max-y)))]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn nearest-object-dist [ind _ objects _]
  (if (zero? (count objects))
    0
    (distance-obj ind (reduce #(if (< (distance-obj ind %1) (distance-obj ind %2)) %1 %2) objects))))

(defn nnd [ind population _ _]
  (reduce
    #(if (and (< %1 (distance ind %2))
              (not (= (:id %2) (:id ind)))
              (not (zero? %1)))
       %1 (distance ind %2))
    (distance ind (first population))
    (rest population))
  #_(second (sort (map #(distance ind %) population))))

(defn osc [ind method]
  (let [min-val -4
        max-val 4
        avg-val (/ (+ min-val max-val) 2)
        amp (/ (- max-val min-val) 2)]
    (+ avg-val (* amp (method (* 0.1 (:age ind) Math/PI 2))))))

(defn osc-sin [ind _ _ _]
  (osc ind #(Math/sin %)))

(defn osc-cos [ind _ _ _]
  (osc ind #(Math/cos %)))


(defn pdr [ind _ _ pheromones]
  (let [ind-x (first (:position ind))
        ind-y (second (:position ind))
        dist-ph #(distance-general
                   ind-x
                   (first (:position %))
                   ind-y
                   (second (:position %)))
        near-ph (filter #(and (not (= (:id %) (:id ind)))
                              (< (- (dist-ph %)) 30))
                        pheromones)]
    (if (zero? (count near-ph))
      0
      (* 10 (reduce #(+ %1 (* (:strength %2) (/ 1 (+ 1 (dist-ph %2)))))
                    (* (:strength (first near-ph))
                       (/ 1 (+ 1 (dist-ph (first near-ph)))))
                    (rest near-ph))))))

(defn dir-to-slope [dir]
  )

(defn position-on-path? [x1 x2 y1 y2 dir]
  (= (/ (- x2 x1) (- y2 y1)) (dir-to-slope dir)))

(defn pdf [ind population _ _]
  (let [ind-x (first (:position ind))
        ind-y (second (:position ind))]
    (distance ind (sort-by #(distance ind %) (filter #(and (not (= (:id ind) (:id %)))
                                                           (= (< ind-x 0) (< ind-y 0))
                                                           ()) population)))))


; motor neuron functions

(defn move-angle [ind dir]
  (let [pos-vec (:position ind)
        angle (* dir 45)
        dist-x (* 5 (Math/round (Math/cos (* angle (/ Math/PI 180)))))
        dist-y (* 5 (Math/round (Math/sin (* angle (/ Math/PI 180)))))]
    (assoc ind :position [(+ (first pos-vec) dist-x) (+ (second pos-vec) dist-y)]
               :direction dir)))

(defn release-pheromone [ind _]
  (assoc ind :pr (not (:pr ind))))

(def sensory-neuron-functions
  {:age  age
   :bdx  bdx
   :bdy  bdy
   :bd   bd
   :oscs osc-sin
   :oscc osc-cos
   :nod  nearest-object-dist
   :pdr pdr
   :dfc  dist-from-center})

(def motor-neuron-functions
  {:mrnd (fn [ind _] (move-angle ind (rand-int 8)))
   :mr   (fn [ind _] (move-angle ind 0))
   :mdr  (fn [ind _] (move-angle ind 1))
   :md   (fn [ind _] (move-angle ind 2))
   :mdl  (fn [ind _] (move-angle ind 3))
   :ml   (fn [ind _] (move-angle ind 4))
   :mul  (fn [ind _] (move-angle ind 5))
   :mu   (fn [ind _] (move-angle ind 6))
   :mur  (fn [ind _] (move-angle ind 7))
   :rlp  release-pheromone})

(def internal-neurons
  {:int1  -1.0
   :int2  -0.8
   :int3  -0.6
   :int4  -0.4
   :int5  -0.2
   :int6  0.2
   :int7  0.4
   :int8  0.6
   :int9  0.8
   :int10 1.0
   })

#_(def internal-neurons
    (select-keys internal-neurons-all (take 6 (shuffle (keys internal-neurons-all)))))


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

(defn neural-paths [ind population objects pheromones]
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
                      (((:source-neuron syn) sensory-neuron-functions) ind population objects pheromones)
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

(defn collate-values [ind population objects pheromones]
  (let [neural-vals (distinct
                      (filter
                        not-empty
                        (flatten-btm-lvl (neural-paths ind population objects pheromones))))]
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
  [ind population objects pheromones]
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
                                                       ((get sensory-neuron-functions %) ind population objects pheromones)
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

(defn calc-motor-output [ind population objects pheromones]
  (let [mot-val-map
        (into {} (for [[k v] (get-weighted-paths ind population objects pheromones)]
                   [k (tanh (apply
                              * (map (fn [syn-seq]
                                       (reduce #(tanh (apply * %1 %2))
                                               (tanh (apply * (first (partition 2 syn-seq))))
                                               (rest (partition 2 syn-seq))))
                                     (flatten-btm-lvl (vector v)))))]))]
    (if (empty? mot-val-map)
      []
      (apply max-key val mot-val-map))))

; ; ---------------- OBJECTS/REDZONES ---------------------

(defn create-object [x y w h]
  {:x x :y y :w w :h h})

(defn create-rand-obj []
  (create-object (rand-nth (range 10 700))
                 (rand-nth (range 10 500))
                 (rand-nth (range 10 60))
                 (rand-nth (range 10 60))))

(defn collided-obj? [ind obj]
  (and (< (first (:position ind)) (+ (:x obj) (:w obj)))
       (> (first (:position ind)) (:x obj))
       (< (second (:position ind)) (+ (:y obj) (:h obj)))
       (> (second (:position ind)) (:y obj))))

(defn collided-ind? [i1 i2]
  (and (not (= (:id i1) (:id i2)))
       (= (:position i1) (:position i2))))

(defn collided-any-obj? [ind objects]
  (> (count (filter #(collided-obj? ind %) objects)) 0))

(defn collided-any-ind? [ind population]
  (> (count (filter #(collided-ind? ind %) population)) 0))

(defn collided-borders? [ind]
  (or (<= (first (:position ind)) 5) (>= (first (:position ind)) 795)
      (<= (second (:position ind)) 5) (>= (second (:position ind)) 595)))

(def example-object-vec
  [{:x 300 :y 0 :w 20 :h 260}
   {:x 300 :y 320 :w 20 :h 280}])

(def red-zones
  [{:x 0 :y 0 :w 800 :h 20}
   #_{:x 0 :y 0 :w 20 :h 600}
   #_{:x 780 :y 0 :w 20 :h 600}
   {:x 0 :y 580 :w 800 :h 20}])

; ---------------- EVOLUTION ---------------------

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


; selection methods

(defn select-right [population]
  (let [filtered-pop (filterv #(> (first (:position %)) 600) population)]
    filtered-pop))

(defn select-left [population]
  (let [filtered-pop (filterv #(< (first (:position %)) 200) population)]
    filtered-pop))

(defn select-circle [population x y]
  (let [radius 100]
    (filterv
      #(< (distance-general
            (first (:position %)) x
            (second (:position %)) y)
          radius)
      population)))

(defn select-central-circle [population]
  (select-circle population 400 300))

(defn select-left-circle [population]
  (select-circle population 200 300))

(defn select-right-circle [population]
  (select-circle population 600 300))

(defn filter-redzones [population redzones]
  (filterv #(not (collided-any-obj? % redzones)) population))

(defn select-method [method]
  (case method
    :right select-right
    :left select-left
    :central-circle select-central-circle
    :right-circle select-right-circle
    :left-circle select-left-circle))


; mutation

(defn repl-mutate [ind]
  (let [new-genome (mapv #(if (< 0.01 (rand))
                            (mutate-hex %)
                            %) (:genome ind))]
    (assoc ind :genome new-genome
               :neural-map (gen-synapse-vec new-genome))))

(defn mutation-method [method]
  (case method
    :replace repl-mutate))

; offspring

(defn make-child [ind id]
  (assoc ind :id id
             :position [(rand-nth (range 5 800 5))
                        (rand-nth (range 5 600 5))]
             :age 0
             :pr false
             :color [(rand-int 170) (rand-int 170) (rand-int 170)]
             :direction -1))

(defn gen-children
  "Generates children, applying selection and mutation"
  [population objects redzones s-method m-method gen-size]
  (loop [children [] child-success 0]
    (if (= gen-size child-success)
      children
      (let [cand-child
            ((mutation-method m-method)
             (make-child
               (rand-nth
                 (filter-redzones
                   ((select-method s-method)
                    population) redzones)) child-success))]
        (if (or (collided-any-ind? cand-child children)
                (collided-any-obj? cand-child objects))
          (recur children child-success)
          (recur (conj children cand-child) (inc child-success)))))))

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
                 (gen-children population example-object-vec red-zones s-method m-method gen-size)))
        (first population)))))

; ---------------- QUIL ---------------------

(defn setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/background 255)
  (let [gen-size 500
        objects (vec example-object-vec
                     #_(concat example-object-vec (repeatedly 10 create-rand-obj)))]
    {:population       (gen-population gen-size 32 objects)
     :objects          objects
     :redzones         red-zones
     :gen-age          0
     :generation       0
     :prev-survivors   0
     :gen-size         gen-size
     :tpg              300
     :selection-method :left
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
    (assoc state
      :population
      (gen-children (:population state)
                    (:objects state)
                    (:redzones state)
                    (:selection-method state)
                    (:mutation-method state)
                    (:gen-size state))
      :gen-age 0
      :generation (inc (:generation state))
      :prev-survivors (count (filter-redzones ((select-method (:selection-method state))
                                               (:population state)) (:redzones state))))))

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
    (q/ellipse (first position) (second position) size size)
    #_(doseq [i (range 10)]
        (let [actual-size (+ size i)]
          (q/fill 100 (/ (:strength pheromone) (inc i)))
          (q/ellipse (first position) (second position) actual-size actual-size)))))

(defn draw-redzone [redzone]
  (q/fill 255 0 0 50)
  (q/rect (:x redzone) (:y redzone) (:w redzone) (:h redzone)))

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
  (doseq [obj (:objects state)]
    (draw-obj obj))
  (doseq [pheromone (:pheromones state)]
    (draw-pheromone pheromone))
  (doseq [ind (:population state)]
    (draw-ind ind))
  (q/fill 0)
  (q/text (str "Generation: " (:generation state)
               "\nPrevious survivors: " (:prev-survivors state)
               "\nPheromone count: " (count (:pheromones state)))
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


(defn -main [view]
  (case view
    :textual (evolve-agents 500 300 12 100 :right :replace)
    :visual (defonce sketch (animate-agents))))

(-main :visual)