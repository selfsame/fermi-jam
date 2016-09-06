Shader "billboard" {
	Properties {
		_MainTex ("_MainTex", 2D) = "white" {
			
		}
		
		_Color ("_Color", Color) = (1,1,1,1)
	}
	SubShader {
		Pass {
			GLSLPROGRAM
			
			#ifdef VERTEX
			
			varying vec4 vTextCoords;
			void main(void){
				(gl_Position = (gl_ModelViewProjectionMatrix * (gl_Vertex * vec4(1.0,1.0,1.0,1.0))));
				(vTextCoords = (gl_Vertex * 0.5));
				
			}
			
			#endif
			
			
			#ifdef FRAGMENT
			
			varying vec4 vTextCoords;
			uniform sampler2D _MainTex;
			uniform vec4 _Color;
			void main(void){
				(gl_FragColor = (_Color * texture2D(_MainTex,vec2(vTextCoords))));
				
			}
			
			#endif
			
			ENDGLSL
		}
		
	}
	
}

