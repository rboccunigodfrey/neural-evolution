(ns neural-evolution.data.neurons.internal)

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
