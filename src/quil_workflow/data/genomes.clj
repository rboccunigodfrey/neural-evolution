(ns quil-workflow.data.genomes
  (:require [clojure.string :as str]))

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
