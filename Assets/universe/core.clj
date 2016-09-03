(ns universe.core
  (use 
    arcadia.core
    arcadia.linear
    hard.core
    hard.seed
    universe.temporal
    universe.spatial))

(def galaxy-count 1e11)

(def TIME (atom 1.38e10))

(defn observable-radius [n]
  (* n (second->year C)))

(def galaxy-seed (atom nil))

(defn rand-galaxy-uid []   
  (long (srand galaxy-count)))

(defn coords [id]
  (seed! id)
  (v3+ (v3 (srand)(srand)(srand)) (v3 -0.5 -0.5 -0.5)))

(noise :universe (coords 6))

(do 
  (clear-cloned!)
  (let [universe-o (name! (clone! :empty) "universe")]
  (dorun
    (map 
      (fn [_]
        (let [gid (rand-galaxy-uid)
              co (v3* (coords gid) 100)]
          (local-scale! (parent! (clone! :sphere co) universe-o) (v3 (* (noise :universe co) 10)))))
      (range 1000)))))

