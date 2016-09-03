(ns universe.core
  (:use 
    arcadia.core
    arcadia.linear
    hard.core
    hard.seed
    hard.mesh
    lines
    universe.domain
    universe.temporal))

(def galaxy-count 1e11)

(def TIME (atom 1.38e10))
(def VIEW-SCALE (atom 100))
(def galaxy-seed (atom nil))

(defn rand-galaxy-uid []   
  (long (srand galaxy-count)))

(defn coords [id]
  (seed! id)
  (v3+ (v3 (srand)(srand)(srand)) (v3 -0.5 -0.5 -0.5)))


(def SPRITES (atom nil))

(defn make-galaxy [p]
  (let [sprites (or @SPRITES (reset! SPRITES (vec (UnityEngine.Resources/FindObjectsOfTypeAll UnityEngine.Sprite))))
        o (clone! :galaxy-medium p)]
        (set! (.sprite (.* o>SpriteRenderer)) (srand-nth sprites))
        o))



(defn make-grid [root n]
  (dorun (for [x (range n) y (range n) z (range n)]
    (parent! (clone! :grid (v3 (- x (int (/ n 2))) (- y (int (/ n 2))) (- z (int (/ n 2))))) root)))
  root)

'(do 
  (seed! (rand))
  (clear-cloned!)
  (let [root (name! (clone! :empty) "universe")
        grid (parent! (name! (clone! :empty) "grid") root)
        galaxies (parent! (name! (clone! :empty) "galaxies") root)
        ]
  (local-scale! (make-grid grid 5) (v3 20))
  (dorun
    (map 
      (fn [_]
        (let [gid (rand-galaxy-uid)
              co (coords gid)]
          (local-scale! 
            (parent! (make-galaxy (v3* co @VIEW-SCALE)) galaxies)
            (v3 (+ 0.5 (srand 2)))) ))
      (range 1000)))))

(first (vertices (first (children (the grid)))))

