Shader "example" {
	Properties {
		_MainTex ("_MainTex", 2D) = "white" {
			
		}
		
		_Color ("_Color", Color) = (1,1,1,1)
	}
	SubShader {
		Pass {
			GLSLPROGRAM
			
			#ifdef VERTEX
			
			varying vec4 vPos;
			varying vec4 vTextCoords;
			void main(void){
				(vPos = gl_Vertex);
				(gl_Position = (gl_ModelViewProjectionMatrix * gl_Vertex));
				(vTextCoords = gl_MultiTexCoord0);
				
			}
			
			#endif
			
			
			#ifdef FRAGMENT
			
			varying vec4 vTextCoords;
			uniform sampler2D _MainTex;
			varying vec4 vPos;
			uniform vec4 _Color;
			void main(void){
				float4 _CosTime;
				(gl_FragColor = ((vPos * (texture2D(_MainTex,vec2(vTextCoords)) + (vec4(_CosTime) * 10.0))) + ((vPos * vec4(_Color[1.0],_Color[2.0],_Color[0.0],1.0)) * _Color)));
				
			}
			
			#endif
			
			ENDGLSL
		}
		
	}
	
}

