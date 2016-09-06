(ns universe.shaders
  (use hard.core arcadia.core)
  (:require 
    [clojure.pprint :as pprint]
    [clojure.walk :as walk]
    [gamma-tools.core :as gamma-tools :refer [g* g- g+ gdiv]]
    [gamma.api :as g]
    [gamma.program :as p]))

(defn- datatype? [v] (or (sequential? v)(map? v)(vector? v)))

(defn- symbol-walk [form xform]
  (cond (symbol? form)   (get xform form form)
        (datatype? form) (clojure.walk/walk #(symbol-walk % xform) identity form)
        :else form))

(def fancies {
  '+ 'g+
  '- 'g-
  '* 'g*
  '/ 'gdiv
  'if 'g/if
  '<  'g/<
  '>  'g/>
  'sin 'g/sin
  'cos 'g/cos
  'tan 'g/tan
  'max 'g/max
  'min 'g/min
  'mod 'g/mod
  'dot 'g/dot
  'pow 'g/pow
  'floor     'g/floor
  'normalize 'g/normalize
  'reflect   'g/reflect
  'vec4      'g/vec4
  'mat4      'g/mat4
  'float     'g/float
  'int       'g/int
  })

(defmacro shady [& forms] `(do ~@(symbol-walk forms fancies)))

(defmacro gl-field [sym idx]
 `(defn ~sym [v#]
    (~'gamma-tools/aget v# ~idx)))

(gl-field x 0)
(gl-field y 1)
(gl-field z 2)
(gl-field w 3)

(defmacro mat-field [sym r c]
  `(defn ~(symbol (str "M" sym)) [m#]
    (~'gamma-tools/aget 
      (~'gamma-tools/aget m# ~r) ~c)))
 

'[a b c d
  e f g h
  i j k l
  m n o p]

(mat-field a 0 0)
(mat-field b 0 1)
(mat-field c 0 2)
(mat-field d 0 3)
(mat-field e 1 0)
(mat-field f 1 1)
(mat-field g 1 2)
(mat-field h 1 3)
(mat-field i 2 0)
(mat-field j 2 1)
(mat-field k 2 2)
(mat-field l 2 3)
(mat-field m 3 0)
(mat-field n 3 1)
(mat-field o 3 2)
(mat-field p 3 3)

(defn billboard-mat [m v]
  (shady 
    (mat4
      (vec4 (Ma m)(Me m)(Mi m) (x v))
      (vec4 (Mb m)(Mf m)(Mj m) (y v))
      (vec4 (Mc m)(Mh m)(Mk m) (z v))
      (vec4 0     0     0         1 ))))

(defn translate-mat [m v]
  (shady 
    (mat4
      (vec4 1     0     0      (x v))
      (vec4 0     1     0      (y v))
      (vec4 0     0     1      (z v))
      (vec4 0     0     0         1 ))))

(defn ->shared-material [o] (.* o>Renderer.sharedMaterial))
(defn ->material [o] (.* o>Renderer.material))
(defn vector! [o name v] (.SetVector (->material o) name v))
(defn color! [o name v] (.SetColor (->material o) name v))
(defn float! [o name v] (.SetFloat (->material o) name v))
(defn float-array! [o name v] (.SetFloatArray o name v))
(defn vector-array! [o name v] (.SetVectorArray o name v))

(defn mat-block [] (UnityEngine.MaterialPropertyBlock. ))
(defn mat-block! [o b] (.SetPropertyBlock (.* o>Renderer) b))

(defn shaders [] (UnityEngine.Resources/FindObjectsOfTypeAll UnityEngine.Shader))



'(let [o (active)
      b (mat-block)]
  (mat-block! o (vector-array! b "uPoints"
    (make-array UnityEngine.Vector4 1000))))

'(.SetGlobalVectorArray (last (shaders)) "uPoints" (make-array UnityEngine.Vector4 100))
'(.SetVectorArray (mat-block) "uPoints" points)
'(vector! (->material (active)) "uPoints0" (Vector4. (rand) 1 1 1))



(def vpos       (g/varying "vPos" :vec4))
(def utint      (assoc (g/uniform "_Color" :vec4) :prop-tag "Color"))
(def utex       (g/uniform "_MainTex" :sampler2D) )
(def vtexcoords (g/varying "vTextCoords" :vec4))

(def gl-position              (g/gl-position))
(def gl-frag-color            (g/gl-frag-color))
(def gl-vertex                {:tag :variable :name "gl_Vertex" :type :vec4})
(def gl-color                 {:tag :variable :name "gl_Color" :type :vec4})
(def gl-mv-projection-matrix  {:tag :variable :name "gl_ModelViewProjectionMatrix" :type :mat4})
(def gl-mv-matrix             {:tag :variable :name "gl_ModelViewMatrix" :type :mat4})
(def unity-t-mv-mat           {:tag :variable :name "UNITY_MATRIX_T_MV" :type :mat4})
(def unity-it-mv-mat          {:tag :variable :name "UNITY_MATRIX_IT_MV" :type :mat4})
(def gl-projection-matrix     {:tag :variable :name "gl_ProjectionMatrix" :type :mat4})
(def gl-v-matrix              {:tag :variable :name "UNITY_MATRIX_VP" :type :mat4})
(def gl-m-matrix              {:tag :variable :name "gl_ModelMatrix" :type :mat4})

(def gl-multi-tex-coords-0    {:tag :variable :name "gl_MultiTexCoord0" :type :vec4})
(def gl-time                  {:tag :variable :name "_Time"   :type :float4})
(def gl-cos-time              {:tag :variable :name "_CosTime" :type :float4})
(def gl-sin-time              {:tag :variable :name "_SinTime" :type :float4})


(defn mget [m c r] (gamma-tools/aget (gamma-tools/aget m c) r))

(do

(defn billboard [] 
   (shady (p/program 
    {:vertex-shader 
      { gl-position 
        (let [mb (billboard-mat gl-mv-projection-matrix gl-vertex)]
         (* gl-mv-projection-matrix (* gl-vertex (vec4 1 1 1 1))) )
       
      vtexcoords
       (* gl-vertex 0.5) }

     :fragment-shader 
     {gl-frag-color 
      (let [tcol (g/texture2D utex (g/vec2 vtexcoords))]
        (* utint tcol))}})))

(gamma-tools/write-shader "billboard" "Assets/shaders" (billboard)))
