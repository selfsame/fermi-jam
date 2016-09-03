(ns universe.temporal)

(defn year->second [n] (/ n 3.154e+7))
(defn second->year [n] (* n 3.154e+7))

(def big-bang 0)

(def lepton-epoch 1.5852885225111E-07)

(def photon-epoch 3.17057704502219E-07)

;cold-dark matter amplifies density variation
(def matter-domination 70000)

;hydrogen and helium atom formation
(def dark-age 377000)

(def stelliferous-era 1e6)

;cosmic expansion between galactic structures becomes faster than light
(def cosmic-light-horizon 100e10)

;background radiation cools
(def cosmic-microwave-cooling 150e10)

(def star-formation-decline 1e12)

;the end of stars
(def degenerate-era 1e14)

;the end of systems and galaxies
(def orbital-entropy 1e19)