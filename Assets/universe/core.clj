(ns universe.core
  (import octree)
  (:use 
    arcadia.core
    arcadia.linear
    hard.core
    hard.seed
    hard.mesh
    tween.core
    hard.gob-pool
    lines
    universe.domain
    universe.temporal
    pdfn.core
    tween.pool))



(gob-pool 2000 Galaxy (local-scale! (-clone :debug-box) (v3 0.5)))

(-stats <>Galaxy)



(defprotocol IResolve
  (coord [o v])
  (-tree [o])
  (-sphere-coords [o v r])
  (-moved? [o v])
  (unresolved [o v r])
  (add [o cs obj])
  (cull [o v r])
  (-update [o v r]))

(extend-type nil
  IResolve
  (coord [o v] nil))


(deftype Resolution [
  ^UnityEngine.GameObject root
  ^System.Double          unit
                          generator
  ^:unsynchronized-mutable origin]
IResolve
(coord ^clojure.lang.PersistentVector [o v]
  [(int (/ (.x v) unit))(int (/ (.y v) unit))(int (/ (.z v) unit))])
(-tree [o] (.pointTree (cmpt root "octree")))
(-moved? [o v]
  (let [co (coord o v)]
    (if (= origin co) false
      (set! origin co))))
(-sphere-coords [o v r]
  (let [[ox oy oz] (coord o v)
        ur (int (/ r unit))]
    (for [x (range (- ur) ur) 
          y (range (- ur) ur) 
          z (range (- ur) ur)
          :when 
          (and (> ur  (.magnitude (v3 x y z)) (* ur 0.9))
            (not (child-named root (str x y z))))]
      [(+ ox x) (+ oy y) (+ oz z)])))
(unresolved [o v r]
  (-sphere-coords o v r))
(add [o [x y z] gob]
  (when gob 
  (let [v (v3* (v3 x y z) unit)]
    (name! gob (str x y z))
  (position! gob v)
  (parent! gob root)
  (.Add (-tree o) gob v))))
(-update [o v r]
  (when (-moved? o v) 
    (dorun 
    (map 
      #(add o % (generator %))
       (unresolved o v r)))
    (dorun (map 
      (fn [gob]
        (.Remove (-tree o) gob)
        (!Galaxy gob))
      (.GetWithout (-tree o) (Ray. v (v3)) (double r)))))))


(defn resolver [name size f]
  (Resolution. 
    (let [o (name! (clone! :empty) (str name))]
      (cmpt+ o octree) o) 
    size f nil))






(def galaxy-count 1e11)

(def TIME (atom 1.38e10))
(def VIEW-SCALE (atom 100))
(def galaxy-seed (atom nil))

(defn rand-galaxy-uid []   
  (long (srand galaxy-count)))

(defn coords [id]
  (seed! id)
  (v3+ (v3 (srand)(srand)(srand)) (v3 -0.5 -0.5 -0.5)))


(defn make-grid [root n]
  (dorun (for [x (range n) y (range n) z (range n)]
    (parent! (clone! :grid (v3 (- x (int (/ n 2))) (- y (int (/ n 2))) (- z (int (/ n 2))))) root)))
  root)



(defn rotater [o]
  (timeline* :loop
    #(do (lerp-look! (the cam) o 0.1)
      (rotate! o (v3 0 0.3 0)))))

(defn cube-v3s 
  ([r] (cube-v3s r nil))
  ([r f]
    (let [df (or f identity)]
    (for [x (range (* r 2))
          y (range (* r 2))
          z (range (* r 2))
          :let [v (v3 (- x r) (- y r) (- z r))]
          :when (df v r)]
      v))))


(defn n->trinoise [n]
  (let [ks [:trinoise1 :trinoise2 :trinoise5 :trinoise8]]
    (get ks (int (* n (count ks))) (first ks))))

  (def mpc-foam 
  (harmonic 
    (noise* (srand) { :out #(* (- (abs %) 0.5) -1)}) [0.228 0.0832 0.1202 1.305] min))


(defn ^UnityEngine.GameObject gen-galaxy [[x y z]]
  (local-scale! (*Galaxy) (v3 3.9))
  #_(let [p (v3 x y z)
        n (mpc-foam p)]
        (when (< n 0.35)
          (local-scale! (create-primitive :sphere) (v3 4))
          #_(let [cnt (srand-int (* (Mathf/Clamp 0 1 n) 4.0))
                o (GameObject. "galaxy")]

          (dorun (map 
            (fn [idx] 
              (let [gv (v3+ p (v3 (?f -3.0 3.0)(?f -3.0 3.0)(?f -3.0 3.0)))]
                (parent! (rotate! (clone! :trinoise5 gv) (v3 (?f 360)(?f 360)(?f 360))) 
                  #_(rotate! 
                    (local-scale! 
                      (clone! (n->trinoise (* cnt 0.1)) gv) (v3 0.4)) 
                    (v3 (?f 360)(?f 360)(?f 360)))
                  o)))
            (range cnt)))
          o) )) )



(do 
  (seed! (srand))
  (mapv destroy [(the xyz)(the universe)(the galaxies)])
  (let [xyz (clone! :xyz)
        camera (clone! :camera)
        root (name! (clone! :empty) "universe")
        grid (parent! (name! (clone! :empty) "grid") root)
        galaxy-res (resolver "galaxies" 5.0 gen-galaxy)
        ]
(def tree (-tree galaxy-res))
(parent! camera xyz)
'(log (with-out-str (time (-update galaxy-res (v3 0.0 0.0 0.0) 20.0))))
'(hook+ root :update (fn [o] (-update galaxy-res (>v3 (the xyz)) 20.0)))
(hook+ root :on-draw-gizmos (fn [o] (.DrawAllBounds (-tree galaxy-res)))))
  )


(.Add tree (clone! :star) (>v3 (the xyz)))


(defn colors [o] (.colors (.mesh (cmpt o UnityEngine.MeshFilter))))

(defn index-vcol [o]
  (let [colors (colors o)]
  (set! (.colors (.* o>MeshFilter.mesh)) 
    (into-array (map-indexed (fn [idx c] (color (int (/ idx 3)) (rand) 0)) 
      colors)))
  (int (/ (count colors) 3))))

'(use 'lines)

(defn add-lines [o]
  (let [lnr (cmpt+ o UnityEngine.LineRenderer)]
  (set-line-renderer-verts lnr (vertices o))))




'[Resolution spec
  :usage
  given
    [a position and radius
     a feature grid size
     ]



]



