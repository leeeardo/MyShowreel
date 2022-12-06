Shader "Custom/grass" {
	Properties {
		_TopColor("Top Color" , Color) = (1,1,1,1)
		_BottomColor("Bottom Color" , Color) = (1,1,1,1)
		
		_NoiseMap("Noise Map" , 2D) = "black"{}
		
		_ColorNoiseStrength("Color Noise Strength" , Range(0,1)) = 0.1
		
		[Header(Animation)]
		_WindSpeed("Wind Speed" , Range(0,5)) = 1
	}
	SubShader {
		Tags {
			"RenderPipeline"="UniversalPipeline"
			"RenderType"="Opaque"
			"Queue"="Geometry"
		}

		HLSLINCLUDE
		#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"

		CBUFFER_START(UnityPerMaterial)
			float4 _TopColor,_BottomColor;
			TEXTURE2D(_NoiseMap);
			float4 _NoiseMap_ST;
			SAMPLER(sampler_NoiseMap);
			float _ColorNoiseStrength;
			float _WindSpeed;
		CBUFFER_END
		ENDHLSL

		Pass {
			Name "ForwardLit"
			Tags { "LightMode"="UniversalForward" }

			Cull Off
			HLSLPROGRAM
			#pragma vertex LitPassVertex
			#pragma fragment LitPassFragment

			// Material Keywords
			#pragma shader_feature_local _NORMALMAP
			#pragma shader_feature_local_fragment _ALPHATEST_ON
			#pragma shader_feature_local_fragment _ALPHAPREMULTIPLY_ON
			#pragma shader_feature_local_fragment _EMISSION
			#pragma shader_feature_local_fragment _METALLICSPECGLOSSMAP
			#pragma shader_feature_local_fragment _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A
			#pragma shader_feature_local_fragment _OCCLUSIONMAP

			#pragma shader_feature_local_fragment _SPECULARHIGHLIGHTS_OFF
			#pragma shader_feature_local_fragment _ENVIRONMENTREFLECTIONS_OFF
			#pragma shader_feature_local_fragment _SPECULAR_SETUP
			#pragma shader_feature_local _RECEIVE_SHADOWS_OFF

			// URP Keywords
			
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN

			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
			#pragma multi_compile_fragment _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING 
			#pragma multi_compile _ SHADOWS_SHADOWMASK 

			// Unity Keywords
			#pragma multi_compile _ LIGHTMAP_ON
			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile_fog

			// TODO GPU Instancing
			#pragma  multi_compile_instancing

			// Includes
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

			// Structs
			struct Attributes {
				float4 positionOS	: POSITION;
				float4 normalOS		: NORMAL;
				float2 uv		    : TEXCOORD0;
				float2 lightmapUV	: TEXCOORD1;
				float4 color		: COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct Varyings {
				float4 positionCS 	: SV_POSITION;
				float2 uv		    : TEXCOORD0;
				DECLARE_LIGHTMAP_OR_SH(lightmapUV, vertexSH, 1);
				float3 normalWS		: TEXCOORD2;
				float3 positionWS	: TEXCOORD3;
				float4 color		: COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			// Textures, Samplers & Global Properties
			

			// Vertex Shader
			Varyings LitPassVertex(Attributes Input) {
				UNITY_SETUP_INSTANCE_ID(Input);
				
				float3 viewerLocal = mul(unity_WorldToObject,float4(_WorldSpaceCameraPos, 1)); 
                float3 localDir = viewerLocal ; //计算新的“forward”
                localDir = normalize(localDir); //归一化。
                float3  upLocal =  float3(0, 1, 0); //默认模型空间的up轴全部为（0,1,0）
                float3  rightLocal = normalize(cross(localDir, upLocal)); //计算新的right轴
                upLocal = cross(rightLocal, localDir); //计算新的up轴。
                float3  newPos = rightLocal * Input.positionOS.x + upLocal * Input.positionOS.y;
				
				
				Varyings Output;

				
				UNITY_TRANSFER_INSTANCE_ID(Input,Output);
				
				VertexPositionInputs positionInputs = GetVertexPositionInputs(Input.positionOS.xyz);
				
				Output.positionWS = positionInputs.positionWS;

				//vertex animation
				float2 noiseUV = positionInputs.positionWS.xz;
				noiseUV+=float2(sin(1*_Time.y+positionInputs.positionWS.z),sin(1*_Time.y+positionInputs.positionWS.x));
				noiseUV*=_WindSpeed;
				float3 noise = SAMPLE_TEXTURE2D_LOD(_NoiseMap,sampler_NoiseMap,0.1*noiseUV,0)*Input.color;
				noise *=0.1;
				newPos+= noise;
				Output.positionCS = TransformObjectToHClip(newPos);
				
				VertexNormalInputs normalInputs = GetVertexNormalInputs(Input.normalOS.xyz);
				Output.normalWS = normalInputs.normalWS;
	
				OUTPUT_LIGHTMAP_UV(Input.lightmapUV, unity_LightmapST, Output.lightmapUV);
				OUTPUT_SH(Output.normalWS.xyz, Output.vertexSH);

				Output.uv = TRANSFORM_TEX(Input.uv,_NoiseMap);
				Output.color = Input.color;
				return Output;
			}

			// Fragment Shader
			half4 LitPassFragment(Varyings Input) : SV_Target {
				
				UNITY_SETUP_INSTANCE_ID(Input);
				

				// Get Baked GI
				half3 bakedGI = SAMPLE_GI(Input.lightmapUV, Input.vertexSH, Input.normalWS);
				
				// Main Light & Shadows
				float4 shadowCoord = TransformWorldToShadowCoord(Input.positionWS.xyz);
				Light mainLight = GetMainLight(shadowCoord);
				half3 attenuatedLightColor = mainLight.color * (mainLight.distanceAttenuation * mainLight.shadowAttenuation);

				// Mix Realtime & Baked (if LIGHTMAP_SHADOW_MIXING / _MIXED_LIGHTING_SUBTRACTIVE is enabled)
				MixRealtimeAndBakedGI(mainLight, Input.normalWS, bakedGI);

				// Diffuse
				half3 shading = bakedGI + attenuatedLightColor;//LightingLambert(attenuatedLightColor, mainLight.direction, Input.normalWS);

				//color noise
				float2 noiseUV = _NoiseMap_ST.xy*Input.positionWS.xz+_NoiseMap_ST.zw;
				noiseUV*=0.05;
				float noise = SAMPLE_TEXTURE2D(_NoiseMap,sampler_NoiseMap,noiseUV);
				noise = noise*(1-_ColorNoiseStrength)+(_ColorNoiseStrength);
				Input.color*=(noise);
				
				half4 color = lerp(_BottomColor,_TopColor,Input.color);
				return half4(color.rgb*shading , color.a);
			}
			ENDHLSL
		}

		
	}
}