Shader "Customs/Tree_Leaf" {
	Properties {
		_TopColor("Top Color" , Color) = (1,1,1,1)
		_BottomColor("Bottom Color" , Color) = (1,1,1,1)
		
		[HideInInspector]_BaseMap ("Leaf Texture", 2D) = "white" {}
		_TrunkMap ("Trunk Texture", 2D) = "white" {}	
		[HideInInspector]_BaseColor("color" , Color) = (1,1,1,1)	
		[Toggle(_ALPHATEST_ON)] _AlphaTestToggle ("Alpha Clipping", Float) = 0
		_Cutoff ("Alpha Cutoff", Float) = 0.5
		
		[Header(Animation)]
		[Toggle(_LEAF_ANIMATE)] _LeafAnimate ("Leaf Animation", Float) = 0
		_NoiseMap("Noise Map" , 2D) = "black"{}
		
		//[Header(AnimationTest)]
		
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
		float4 _BaseMap_ST;
		float4 _TrunkMap_ST;
		float4 _NoiseMap_ST;
		float _Cutoff;
		float4 _BaseColor;
		float4 _TopColor,_BottomColor;
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

			#pragma shader_feature_local_vertex _LEAF_ANIMATE
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
			
			// Includes
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

			// Structs
			struct Attributes {
				float4 positionOS	: POSITION;
				float4 normalOS		: NORMAL;
				float2 uv		    : TEXCOORD0;
				float2 lightmapUV	: TEXCOORD1;
				float4 color		: COLOR;
			};

			struct Varyings {
				float4 positionCS 	: SV_POSITION;
				float2 uv		    : TEXCOORD0;
				DECLARE_LIGHTMAP_OR_SH(lightmapUV, vertexSH, 1);
				float3 normalWS		: TEXCOORD2;
				float3 positionWS	: TEXCOORD3;
				float4 color		: COLOR;
			};

			// Textures, Samplers & Global Properties
			TEXTURE2D(_BaseMap);
			SAMPLER(sampler_BaseMap);
			TEXTURE2D(_TrunkMap);
			SAMPLER(sampler_TrunkMap);

			TEXTURE2D(_NoiseMap);
			SAMPLER(sampler_NoiseMap);

			// Vertex Shader
			Varyings LitPassVertex(Attributes IN) {
				Varyings OUT;

				VertexPositionInputs positionInputs = GetVertexPositionInputs(IN.positionOS.xyz);
				
				
				//OUT.positionCS = positionInputs.positionCS;
				OUT.positionWS = positionInputs.positionWS;

				float3 noise = 0;
				
					float2 noiseUV = OUT.positionWS.xz;
					noiseUV-=float2(sin(1*_Time.y+positionInputs.positionWS.z),sin(1*_Time.y+positionInputs.positionWS.x));
					float colorRamp = IN.color.g;
					noiseUV*=colorRamp;
				noise = 0.03*SAMPLE_TEXTURE2D_LOD(_NoiseMap,sampler_NoiseMap,0.1*noiseUV,0)*(1-step(IN.color.g,0.2));
					
				
				#if defined(_LEAF_ANIMATE)
					IN.positionOS.xyz-=noise;
				#endif
				OUT.positionCS = TransformObjectToHClip(IN.positionOS.xyz);

				VertexNormalInputs normalInputs = GetVertexNormalInputs(IN.normalOS.xyz);
				OUT.normalWS = normalInputs.normalWS;

				OUTPUT_LIGHTMAP_UV(IN.lightmapUV, unity_LightmapST, OUT.lightmapUV);
				OUTPUT_SH(OUT.normalWS.xyz, OUT.vertexSH);

				OUT.uv = TRANSFORM_TEX(IN.uv, _BaseMap);
				OUT.color = IN.color;
				return OUT;
			}

			// Fragment Shader
			half4 LitPassFragment(Varyings IN) : SV_Target {
				half4 leafMap = SAMPLE_TEXTURE2D(_BaseMap, sampler_BaseMap, IN.uv);
				half4 truckMap = SAMPLE_TEXTURE2D(_TrunkMap, sampler_TrunkMap, IN.uv);

				#ifdef _ALPHATEST_ON
					// Alpha Clipping
					clip(leafMap.a - _Cutoff);
				#endif

				// Get Baked GI
				half3 bakedGI = SAMPLE_GI(IN.lightmapUV, IN.vertexSH, IN.normalWS);
				
				// Main Light & Shadows
				float4 shadowCoord = TransformWorldToShadowCoord(IN.positionWS.xyz);
				Light mainLight = GetMainLight(shadowCoord);
				half3 attenuatedLightColor = mainLight.color * (mainLight.distanceAttenuation * mainLight.shadowAttenuation);

				// Mix Realtime & Baked (if LIGHTMAP_SHADOW_MIXING / _MIXED_LIGHTING_SUBTRACTIVE is enabled)
				MixRealtimeAndBakedGI(mainLight, IN.normalWS, bakedGI);

				// Diffuse
				half3 shading = bakedGI + LightingLambert(attenuatedLightColor, mainLight.direction, IN.normalWS);

				float colorRamp = (IN.color.g-0.5)*2;
				half3 leafColor = lerp(_BottomColor.rgb,_TopColor.rgb,colorRamp)*(1-step(IN.color.g,0.2));
				//half4 leafColor = leafMap *(1-step(IN.color.g,0.2));
				half3 trunkColor = max(0,truckMap.rgb * (1-step(IN.color.b,0.2)));

				//return float4(trunkColor.rgb*shading,1);
				return half4((leafColor.rgb+trunkColor.rgb) * shading, 1);
			}
			ENDHLSL
		}
		
		// ShadowCaster, for casting shadows
		Pass {
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual

			HLSLPROGRAM
			#pragma vertex ShadowPassVertex
			#pragma fragment ShadowPassFragment

			// Material Keywords
			#pragma shader_feature_local_fragment _ALPHATEST_ON
			#pragma shader_feature_local_fragment _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			// GPU Instancing
			#pragma multi_compile_instancing

			#pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW
			
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/CommonMaterial.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/SurfaceInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Shaders/ShadowCasterPass.hlsl"
			
			ENDHLSL
		}

		// DepthOnly, used for Camera Depth Texture (if cannot copy depth buffer instead, and the DepthNormals below isn't used)
		Pass {
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ColorMask 0
			ZWrite On
			ZTest LEqual

			HLSLPROGRAM
			#pragma vertex DepthOnlyVertex
			#pragma fragment DepthOnlyFragment

			// Material Keywords
			#pragma shader_feature_local_fragment _ALPHATEST_ON
			#pragma shader_feature_local_fragment _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			// GPU Instancing
			#pragma multi_compile_instancing

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/CommonMaterial.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/SurfaceInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Shaders/DepthOnlyPass.hlsl"
			
			ENDHLSL
		}

		// DepthNormals, used for SSAO & other custom renderer features that request it
		Pass {
			Name "DepthNormals"
			Tags { "LightMode"="DepthNormals" }

			ZWrite On
			ZTest LEqual

			HLSLPROGRAM
			#pragma vertex DepthNormalsVertex
			#pragma fragment DepthNormalsFragment

			// Material Keywords
			#pragma shader_feature_local _NORMALMAP
			#pragma shader_feature_local_fragment _ALPHATEST_ON
			#pragma shader_feature_local_fragment _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			// GPU Instancing
			#pragma multi_compile_instancing
			//#pragma multi_compile _ DOTS_INSTANCING_ON

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/CommonMaterial.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/SurfaceInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Shaders/DepthNormalsPass.hlsl"
			
			ENDHLSL
		}
		
	}
}