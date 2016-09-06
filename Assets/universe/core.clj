(ns universe.core
  (import octree)
  (:use 
    arcadia.core
    arcadia.linear
    hard.core
    hard.input
    hard.seed
    hard.mesh
    tween.core
    hard.gobpool
    lines
    universe.domain
    universe.temporal
    pdfn.core
    tween.pool))


(destroy-tagged "clone")
(clear-cloned!)


(gobpool 2 Galaxy (local-scale! (-clone :star) (v3 4.0)))

(-stats <>Galaxy)



(defprotocol IResolve
  (coord [o v])
  (-tree [o])
  (-sphere-coords [o v r])
  (-moved? [o v])
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
(coord [o v]
  (v3 (* (Mathf/Floor (.x v)) (/ 1 unit))
      (* (Mathf/Floor (.y v)) (/ 1 unit))
      (* (Mathf/Floor (.z v)) (/ 1 unit))))
(-tree [o] (.pointTree (cmpt root "octree")))
(-moved? [o v] true)
(-sphere-coords [o v r]
  (let [cv (coord o v)
        ur (int r)]
    (for [x (range (- ur) ur) 
          y (range (- ur) ur) 
          z (range (- ur) ur)]
      (v3 (+ 0.1 x (.x cv))(+ 0.1 y (.y cv))(+ 0.1 z (.z cv))))))
(add [o [x y z] gob])
(-update [o v r]
  (when (-moved? o v) 
    (let [cv (v3+ v (v3 (?f (- r) r)(?f (- r) r)(?f (- r) r)))]
    ;(.Remove (-tree o) (the xyz))
    ;(.Add (-tree o) (the xyz) (>v3 (the xyz)))
    (if-not 
      (first (.GetNearby  (-tree o) (Ray. cv
       (v3 0.01)) (double unit)))

  
  (.Add (-tree o) (parent! (rotate! (position! (clone! :star) cv) (v3 (?f 360)(?f 360)(?f 360))) root) cv)
    (dorun (map 
      (fn [gob]

        (try (.Remove (-tree o) gob)
             (catch Exception e (log (prn-str [(>v3 gob) gob v])))
             (finally (destroy gob))))
      (.GetWithout (-tree o) (Ray. v (v3 0.01)) (double (*  r 2.0)))))) ))))


(clone! :xyz)
(clone! :quadrant)

(defn resolver [name size f]
  (Resolution. 
    (let [o (name! (clone! :xyz) (str name))]
      (cmpt+ o octree)
      (position! o (v3)) o) 
    size f nil))

;(benchmark 1000 (aget (.GetWithout  (-tree t) (Ray. (v3 0.0) (v3 0.01)) (double 3)) 0))

(defn n->trinoise [n]
  (let [ks [:trinoise1 :trinoise2 :trinoise5 :trinoise8]]
    (get ks (int (* n (count ks))) (first ks))))

  (def mpc-foam 
  (harmonic 
    (noise* (srand) { :out #(* (- (abs %) 0.5) -1)}) [0.228 0.0832 0.1202 1.305] min))


(defn ^UnityEngine.GameObject gen-galaxy [p]
  (let [n (mpc-foam p)]
        (clone! :star)
        #_(when (< n 0.35)
          (clone! :star)
          #_(let [cnt (srand-int (* (Mathf/Clamp 0 1 n) 4.0))
                o (GameObject. "galaxy")]

          (dorun (map 
            (fn [idx] 
              (let [gv (v3+ p (v3 (?f -3.0 3.0)(?f -3.0 3.0)(?f -3.0 3.0)))]
                (parent! (rotate! (clone! :star gv) (v3 (?f 360)(?f 360)(?f 360))) 
                  #_(rotate! 
                    (local-scale! 
                      (clone! (n->trinoise (* cnt 0.1)) gv) (v3 0.4)) 
                    (v3 (?f 360)(?f 360)(?f 360)))
                  o)))
            (range cnt)))
          o) )) )




(def t (resolver "joe" 4.0 (fn [cr] (gen-galaxy cr) )))

(hook+ (the xyz) :update (fn [o] (-update t (v3* (>v3 o) 1.0) 10.0)))
(hook+ (.root t) :on-draw-gizmos (fn [o] (if true (.DrawAllBounds (-tree t)))))


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
      (if (key? "p") (set! (.* (the cam)>Camera.clearFlags) UnityEngine.CameraClearFlags/Depth))
      (if (key? "o") (set! (.* (the cam)>Camera.clearFlags) UnityEngine.CameraClearFlags/Skybox))
      (if (key? "l") (update-state! o :speed inc))
      (if (key? "k") (update-state! o :speed dec))
      (if (key? "s") (position! (the cam) (v3- (>v3 (the cam)) (v3 0 0 0.2))))
      (rotate! o (v3 0 (* 0.05 (or (state o :speed) 10)) 0)))))

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






(do 
  (seed! (srand))
  (let [camera (clone! :camera)
        root (name! (clone! :empty) "universe")
        grid (parent! (name! (clone! :empty) "grid") root)
        ;galaxy-res (resolver "galaxies" 5.0 gen-galaxy)
        ]
        (state! camera {:speed 12})
;(def tree (-tree galaxy-res))
(parent! camera (the xyz))
'(log (with-out-str (time (-update galaxy-res (v3 0.0 0.0 0.0) 20.0))))
'(hook+ root :update (fn [o] (-update galaxy-res (>v3 (the xyz)) 20.0)))
'(hook+ root :on-draw-gizmos (fn [o] (.DrawAllBounds (-tree galaxy-res)))))
  )



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



(map #(timeline* :loop (tween {:local {:scale (v3 (?f 1 0.2))}} % (?f 0.3 0.2)))
  (every star))


(map #(timeline* :loop (tween {:material {:color (color (?f)(?f)(?f))}} % (?f 0.01 0.1)))
  (every line-sphere))

(use 'hard.edit)
(hook+ (active) :update (fn [o] (rotate! o (v3 0 0 (* -0.01 (or (state o :speed) 10))))))