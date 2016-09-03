(ns universe.shaders
  (:require 
    [clojure.pprint :as pprint]
    [gamma-tools.core :as gamma-tools]
    [gamma.api :as g]
    [gamma.program :as p]))


(defmacro gl-field [sym idx]
 `(defn ~sym [v#]
    (~'gamma.api/aget v# ~idx)))

(gl-field g-x 0)
(gl-field g-y 1)
(gl-field g-z 2)
(gl-field g-w 3)
(gl-field g-r 0)
(gl-field g-g 1)
(gl-field g-b 2)
(gl-field g-a 3)

 



(def vpos       (g/varying "vPos" :vec4))
(def utint      (assoc (g/uniform "_Color" :vec4) :unity-type :color))
(def utex       (g/uniform "_MainTex" :sampler2D) )
(def vtexcoords (g/varying "vTextCoords" :vec4))

(def gl-vertex                {:tag :variable :name "gl_Vertex" :type :vec4})
(def gl-mv-projection-matrix  {:tag :variable :name "gl_ModelViewProjectionMatrix" :type :mat4})
(def gl-multi-tex-coords-0    {:tag :variable :name "gl_MultiTexCoord0" :type :vec4})
(def gl-time                  {:tag :variable :name "_Time" :type :float4})
(def gl-cos-time              {:tag :variable :name "_CosTime" :type :float4})
(def gl-sin-time              {:tag :variable :name "_SinTime" :type :float4})



(defn example []
  (p/program
    {:vertex-shader {
      vpos             gl-vertex
      (g/gl-position)  (g/* gl-mv-projection-matrix  gl-vertex)
      vtexcoords       gl-multi-tex-coords-0}
     :fragment-shader {
      (g/gl-frag-color) 
      (let [tcol (g/texture2D utex (g/vec2 vtexcoords))
            trgb (g/* (g-r tcol) (g-g tcol))
            dt (g/vec4 gl-cos-time)
            t2 (g/vec4 (g-g utint)(g-b utint)(g-r utint) 1.0)]
        (g/+ (g/* vpos (g/+ tcol (g/* dt 10.0)))
          (g/* (g/* vpos t2) utint)
          ))}}))

(gamma-tools/write-shader "example" "Assets/shaders" (example))






'(pprint/pprint (example))
'(print (gamma-tools/unity-shader "example"  (exampl)))