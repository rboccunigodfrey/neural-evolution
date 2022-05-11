(ns neural-evolution.evolution.mutation
  (:require [clojure.string :as str])
  (:use [neural-evolution.data.synapses]))

; mutation

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

(defn repl-mutate [ind]
  (let [new-genome (mapv #(if (< 0.02 (rand))
                            (mutate-hex %)
                            %) (:genome ind))]
    (assoc ind :genome new-genome
               :neural-map (gen-synapse-vec new-genome))))

(defn mutation-method [method]
  (case method
    :replace repl-mutate))