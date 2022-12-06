Shader "Custom/Cloud" {
	Properties {
		_BaseMap ("Cloud Texture", 2D) = "white" {}
		_NoiseMap ("Noise Texture", 2D) = "black" {}
		_DistortionSpeedX("DistortionSpeedX" , float) = 1
		_DistortionSpeedY("DistortionSpeedY" , float) = 1
		_DistortionIntensity("Distortion Intensity" , float) = 1
		//_BaseColor ("Example Colour", Color) = (0, 0.66, 0.73, 1)
		[HDR]_TopColor("Top Color" , Color) = (1,1,1,1)
		[HDR]_RimLight("Rim Light" , Color) = (1,1,1,1)
		[HDR]_BottonColor("Botton Color" , Color) = (1,1,1,1)
		_Power("X=Top,Y=Botton,Z=Rim" , Vector) = (1,1,1,1)
		
		
	}
	SubShader {
		Tags {
			"RenderPipeline"="UniversalPipeline"
			"RenderType"="Transparent"
			"Queue"="Transparent"
		}

		HLSLINCLUDE
		#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"

		CBUFFER_START(UnityPerMaterial)
		float4 _BaseMap_ST;
		//float4 _BaseColor;
		float4 _TopColor,_BottonColor,_RimLight;
		float4 _Power;
		float _DistortionIntensity,_DistortionSpeedX,_DistortionSpeedY;
		CBUFFER_END
		ENDHLSL

		Pass {
			Name "ForwardLit"
			Tags { "LightMode"="UniversalForward" }

			Blend SrcAlpha OneMinusSrcAlpha
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
				//float4 color		: COLOR;
			};

			// Textures, Samplers & Global Properties
			TEXTURE2D(_BaseMap);
			SAMPLER(sampler_BaseMap);
			TEXTURE2D(_NoiseMap);
			SAMPLER(sampler_NoiseMap);
			// Vertex Shader
			Varyings LitPassVertex(Attributes Input) {
				//float3 centerOffset = float3(0, 0, 0);
                //Input.positionOS.xyz -= centerOffset;
                float3 viewerLocal = mul(unity_WorldToObject,float4(_WorldSpaceCameraPos, 1)); //将摄像机的坐标转换到物体模型空间
                float3 localDir = viewerLocal ; //计算新的“forward”
                localDir.y = 0; //这里有两种方式，一种是仰面也要对齐，涉及XYZ面，另外一种就是只考虑XY面，即把y值置0。
                localDir = normalize(localDir); //归一化。
                float3  upLocal =  float3(0, 1, 0); //默认模型空间的up轴全部为（0,1,0）
                float3  rightLocal = normalize(cross(localDir, upLocal)); //计算新的right轴
                upLocal = cross(rightLocal, localDir); //计算新的up轴。
                float3  BBLocalPos = rightLocal * Input.positionOS.x + upLocal * Input.positionOS.y; //将原本的xy坐标以在新轴上计算，相当于一个线性变换【原模型空间】->【新模型空间】
				

                Varyings o;
                o.positionCS = TransformObjectToHClip(BBLocalPos);
				VertexNormalInputs normalInputs = GetVertexNormalInputs(Input.normalOS.xyz);
				o.normalWS = normalInputs.normalWS;
				o.positionWS = TransformObjectToWorld(Input.positionOS);
				OUTPUT_LIGHTMAP_UV(IN.lightmapUV, unity_LightmapST, OUT.lightmapUV);
				OUTPUT_SH(o.normalWS.xyz, o.vertexSH);
                o.uv = Input.uv;
                return o;
				
			}

			// Fragment Shader
			half4 LitPassFragment(Varyings Input) : SV_Target
			{
				half4 noiseMap = SAMPLE_TEXTURE2D(_NoiseMap,sampler_NoiseMap,Input.uv+_Time.y*float2(-0.01*_DistortionSpeedX,-0.01*_DistortionSpeedY));
				half noiseMask = SAMPLE_TEXTURE2D(_NoiseMap,sampler_NoiseMap,Input.uv).r;
				noiseMap.rg*=max(noiseMask,0.5);
				half4 baseMap = SAMPLE_TEXTURE2D(_BaseMap, sampler_BaseMap, Input.uv+0.01*_DistortionIntensity*noiseMap);
				
				float3 topColor = lerp(0,_TopColor,baseMap.r).rgb;
				topColor = pow(topColor,_Power.x);
				
				float3 bottonColor = lerp(0,_BottonColor,baseMap.g).rgb;
				bottonColor = pow(bottonColor,_Power.y);
				float3 rimColor = lerp(0,_RimLight,baseMap.b).rgb;
				rimColor = pow(rimColor,_Power.z);
				
				float3 baseColor = saturate(topColor+bottonColor+rimColor);

				// Get Baked GI
				half3 bakedGI = SAMPLE_GI(Input.lightmapUV, Input.vertexSH, Input.normalWS);
				
				// Main Light & Shadows
				float4 shadowCoord = TransformWorldToShadowCoord(Input.positionWS.xyz);
				Light mainLight = GetMainLight(shadowCoord);
				half3 attenuatedLightColor = mainLight.color * (mainLight.distanceAttenuation * mainLight.shadowAttenuation);

				// Mix Realtime & Baked (if LIGHTMAP_SHADOW_MIXING / _MIXED_LIGHTING_SUBTRACTIVE is enabled)
				MixRealtimeAndBakedGI(mainLight, Input.normalWS, bakedGI);
				
				
				// Diffuse
				half3 color =baseColor+bakedGI;
				
				//test
				half3 normal = baseMap.bgr;
				Light mainlight = GetMainLight();
				float NdotL = dot(-normal,mainLight.direction);
				NdotL = NdotL*0.5+1;
				//return half4(NdotL,NdotL,NdotL,baseMap.a);
				return half4(color.rgb*NdotL,pow(baseMap.a,0.5));
			}
			ENDHLSL
		}
		
	}
}