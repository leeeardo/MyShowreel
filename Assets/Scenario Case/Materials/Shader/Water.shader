Shader "Customs/Water" {
	Properties {
		_BaseMap ("Color Texture", 2D) = "white" {}
		[HideInInspector]_BaseColor ("Base Colour", Color) = (0, 0.66, 0.73, 1)
		_ShadowStrength("ShadowStrength" , Range(0,1)) = 1
		_ReflectStrength("ReflectStrength" , Range(0,1)) = 1
		[Header(About Depth)]
		_DepthDistance("Depth Distance" , Range(0,1)) = 1
		
		[Header(About Normal)]
		_NormalSpeed("Speed" , Range(-2,2)) = 0.05
		_NormalScale("Scale" , vector) = (0,0,0,0)
		_NormalMap("Normal Map" , 2D) = "bump"{}
		_NormalStrength("Normal Strength" , float) = 1
		_DetailNormalMap("Detail Normal Map" , 2D) = "bump"{}
		_DetailNormalStrength("Detail Normal Strength" , float) = 1
		
		_RefractionStrength("RefractionStrength", float) = 1
		
		[Header(AboutFoam)]
		_FoamAmount("Foam Amount" , float) = 1
		_FoamCutoff("Foam Cutoff" , float) = 1
		[HDR]_FoamColor ("Foam Colour", Color) = (1,1,1,1)
		_FoamSpeed("Foam Speed" , float) = 1
		_FoamScale("Foam Scale" , float) = 1
		_FoamNoise("Foam Noise Tex" , 2D) = "white"{}
		[Header(AboutSmallFoam)]
		_SmallFoamAmount("Small Foam Amount" , Range(0,5)) = 1
        _SmallFoamCutoff("Small Foam Cutoff" , Range(0,1)) = 1
		
		[Header(AboutWave)]
		_WaveSpeed("Wave Speed" , float) = 1
		_WaveFrequency("Wave Frequency" , float) = 1
		_WaveAmplitude("Wave Amplitude" , float) = 1
		
		[Header(AboutCaustics)]
		_CausticsTexture("Caustics Texture" , 2D) = "black"{}
		[HDR]_CausticsTint("Caustics Tint", Color) = (1,1,1,1)
		_CauticsRange("Cautics Range" , Range(0,50)) = 5
		_CausticsDistort("Caustics Distort" , float) = 1
		_CausticsSpeed("Caustics Speed" , vector) = (0,0,0,0)
		
		_ParamsA("Gerster ParamsA",vector) = (0,0,0,0)
		_ParamsB("Gerster ParamsB",vector) = (0,0,0,0)
		_ParamsC("Gerster ParamsC",vector) = (0,0,0,0)
		[HideInInspector]
		_Cutoff("Cut Off",float) = 1
		
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
		float4 _BaseColor;
		float4 _CausticsTexture_ST;
		float _DepthDistance;
		float _Cutoff;
		float _NormalStrength,_DetailNormalStrength;
		float _NormalSpeed,_RefractionStrength;
		float4 _NormalScale,_FoamColor;
		float _FoamAmount,_FoamCutoff,_FoamSpeed,_FoamScale,_SmallFoamAmount,_SmallFoamCutoff;
		float _WaveSpeed,_WaveFrequency , _WaveAmplitude;
		float _ShadowStrength , _ReflectStrength,_CausticsDistort , _CauticsRange;
		float4 _CausticsSpeed;
		float4 _CausticsTint;
		float4 _ParamsA,_ParamsB,_ParamsC;
		CBUFFER_END
		ENDHLSL

		Pass {
			Name "ForwardLit"
			Tags { "LightMode"="UniversalForward" }

			//Cull Off
			Blend SrcAlpha OneminusSrcAlpha
			
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
				float4 tangentOS	: TANGENT;
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
				float4 positionSS	: TEXCOORD4;
				float3 tangentWS	: TEXCOORD5;
				float3 bitangentWS	: TEXCOORD6;
				float4 color		: COLOR;
			};

			// Textures, Samplers & Global Properties
			TEXTURE2D(_BaseMap);
			SAMPLER(sampler_BaseMap);
			TEXTURE2D(_CameraDepthTexture);
			SAMPLER(sampler_CameraDepthTexture);
			TEXTURE2D(_CameraOpaqueTexture);
			SAMPLER(sampler_CameraOpaqueTexture);
			TEXTURE2D(_NormalMap);
			SAMPLER(sampler_NormalMap);
			TEXTURE2D(_DetailNormalMap);
			SAMPLER(sampler_DetailNormalMap);
			TEXTURE2D(_FoamNoise);
			SAMPLER(sampler_FoamNoise);
			TEXTURE2D(_CausticsTexture);
			SAMPLER(sampler_CausticsTexture);

			void GerstnerWave_float (
				    float4 waveDir, float4 waveParam, float3 p, out float3 delta_pos, out float3 delta_normalWS
				) {
				    // waveParam : steepness, waveLength, speed, amplify
				    float steepness = waveParam.x;
				    float wavelength = waveParam.y;
				    float speed = waveParam.z;
				    float amplify = waveParam.w;
				    float2 d = normalize(waveDir.xz);

				    float w = 2 * 3.1415 / wavelength;
				    float f = w * (dot(d, p.xz) - _Time.y * speed);
				    float sinf = sin(f);
				    float cosf = cos(f);

				    steepness = clamp(steepness, 0, 1 / (w*amplify));

				    delta_normalWS = float3(
				        - amplify * w * d.x * cosf,
				        - steepness * amplify * w * sinf,
				        - amplify * w * d.y * cosf
				    );

				    delta_pos = float3(
				        steepness * amplify * d.x * cosf,
				        amplify * sinf,
				        steepness * amplify * d.y * cosf
				    );
				}
			
			float3 GetWorldPosFromDepth(float4 positionSS,float3 worldPos)
			{
				float4 depthMap = SAMPLE_TEXTURE2D(_CameraDepthTexture,sampler_CameraDepthTexture,positionSS.xy/positionSS.w);
				float eyeDepth = LinearEyeDepth(depthMap.r,_ZBufferParams);
				float3 CamPos = GetCameraPositionWS();
				
				float3 viewDir = normalize(GetWorldSpaceViewDir(worldPos));
				float3 ray = viewDir/
					dot(viewDir,-1 * mul(
						UNITY_MATRIX_M, transpose(
							mul(UNITY_MATRIX_I_M, UNITY_MATRIX_I_V)) [2].xyz));
				return CamPos+eyeDepth*ray;
			}

			
			// Vertex Shader
			Varyings LitPassVertex(Attributes Input) {
				Varyings Output;
				
				//Wave Animation 1PlaneVersion
				//float offsetOS = _WaveAmplitude*sin(_WaveSpeed*_Time.y + Input.positionOS.x*_WaveFrequency);
				//Input.positionOS.y+=offsetOS;
				//Wave Animation muiltPlaneVersion
				
				
				float3 worldPos = TransformObjectToWorld(Input.positionOS);
				float3 normalWS = TransformObjectToWorldNormal(Input.normalOS);
				float offsetOS = _WaveAmplitude*sin(_WaveSpeed*_Time.y + worldPos.z*_WaveFrequency);
				
				
				float3 delta_pos , delta_normalWS;
				float3 osNormal=normalWS;
				GerstnerWave_float(float4(0,1,1,1),_ParamsA,worldPos,delta_pos,delta_normalWS);
				Input.positionOS.xyz+=delta_pos;
				osNormal+=delta_normalWS;
				GerstnerWave_float(float4(0.2,1,1,1),_ParamsB,worldPos,delta_pos,delta_normalWS);
				Input.positionOS.xyz+=delta_pos;
				osNormal+=delta_normalWS;
				GerstnerWave_float(float4(-0.8,1,1,1),_ParamsC,worldPos,delta_pos,delta_normalWS);
				Input.positionOS.xyz+=delta_pos;
				osNormal+=delta_normalWS;
				osNormal=normalize(osNormal);
				VertexPositionInputs positionInputs = GetVertexPositionInputs(Input.positionOS.xyz);
				Output.positionCS = positionInputs.positionCS;
				Output.positionWS = positionInputs.positionWS;
				Output.positionSS = positionInputs.positionNDC;

				Input.normalOS.xyz= normalize(TransformWorldToObjectNormal(osNormal));
				//normalInputs.normalWS;
				VertexNormalInputs normalInputs = GetVertexNormalInputs(Input.normalOS.xyz);
				Output.normalWS = normalInputs.normalWS;
				Output.tangentWS = normalInputs.tangentWS;
				Output.bitangentWS = normalInputs.bitangentWS;

				OUTPUT_LIGHTMAP_UV(Input.lightmapUV, unity_LightmapST, Output.lightmapUV);
				OUTPUT_SH(Output.normalWS.xyz, Output.vertexSH);

				
				
				Output.uv = Input.uv;
				Output.color = Input.color;
				return Output;
			}

			// Fragment Shader
			half4 LitPassFragment(Varyings Input) : SV_Target {
				float3 normalWS = NormalizeNormalPerPixel(Input.normalWS);
				//return float4(dot(normalWS,GetMainLight().direction).xxx,1);
				float3 tangentWS = normalize(Input.tangentWS);
				float3 bitangentWS = normalize(Input.bitangentWS);
				float3x3 TBN = {tangentWS,bitangentWS,normalWS};
				
				//深度图采样调整
				half4 depthMap = SAMPLE_TEXTURE2D(_CameraDepthTexture,sampler_CameraDepthTexture,Input.positionSS.xy/Input.positionSS.w);
				half cameraDepth = Linear01Depth(depthMap.r,_ZBufferParams);
				//cameraDepth/=_DepthDistance;
				half depth = saturate((cameraDepth-Input.positionCS.z)/_DepthDistance);

				//基于深度的ramp采样
				half4 colormap = SAMPLE_TEXTURE2D(_BaseMap, sampler_BaseMap, depth.xx);

				//NormalTillintAndOffset
				float2 uvOffsetA = Input.uv*_NormalScale.xy+_Time.y*_NormalSpeed;
				float2 uvOffsetB = Input.uv*_NormalScale.zw-_Time.y*_NormalSpeed;
				
				float3 mainNormalMap = UnpackNormalScale(SAMPLE_TEXTURE2D(
					_NormalMap,sampler_NormalMap,Input.uv+uvOffsetA),_NormalStrength);
				float3 normalMain = normalize(mul(mainNormalMap,TBN));
				float3 detailNormalMap = UnpackNormalScale(SAMPLE_TEXTURE2D(
					_DetailNormalMap,sampler_DetailNormalMap,Input.uv+uvOffsetB),_DetailNormalStrength);
				float3 normalDetail = normalize(mul(detailNormalMap,TBN));
				float3 normal = BlendNormalWorldspaceRNM(normalMain,normalDetail,normalWS);

				normal = lerp(normal,Input.normalWS,depth);

				//Refraction
				float2 cameraUV = (Input.positionSS.xy/Input.positionSS.w);
				cameraUV.xy+=_RefractionStrength*0.002*normal;
				half4 cameraColorTex = SAMPLE_TEXTURE2D(_CameraOpaqueTexture,sampler_CameraOpaqueTexture,cameraUV);

				//Foam
				half eyeDepth = LinearEyeDepth(depthMap.r,_ZBufferParams);
				float2 foamUV = Input.uv * _FoamScale+_Time.y* _FoamSpeed;
 				float foamNoise = SAMPLE_TEXTURE2D(_FoamNoise,sampler_FoamNoise,0.2*Input.positionWS.xz);
				float foamDepth = saturate((eyeDepth-Input.positionSS.w)/_FoamAmount)*_FoamCutoff;
				float foamNoiseA = step(foamDepth,foamNoise);
				float foam = foamNoiseA*_FoamColor.a*(1-depth);
				
				//Foam2
				//float foamBDepth = saturate((depth-Input.positionCS.z)/_SmallFoamAmount*_SmallFoamCutoff);
				float foamNoiseB = step(foamNoise,_SmallFoamCutoff);
				float mask = SAMPLE_TEXTURE2D(_CausticsTexture,sampler_CausticsTexture,0.05*Input.positionWS.xz);
				
				foam+=mask*smoothstep(0.05,0.1,saturate(Input.positionWS.y-48.5))*(1-foamNoiseB);

				//Fresnel
				float fresnel = 1-saturate(0.5+0.5*dot(GetWorldSpaceViewDir(Input.positionWS),normal));
				
				//ReflectionProbe
				float3 reflectDir = reflect(-GetWorldSpaceViewDir(Input.positionWS), normal);
				reflectDir = BoxProjectedCubemapDirection(reflectDir,Input.positionWS,unity_SpecCube0_ProbePosition,
					unity_SpecCube0_BoxMin,unity_SpecCube0_BoxMax);
				half4 rgbm = SAMPLE_TEXTURECUBE_LOD(unity_SpecCube0,samplerunity_SpecCube0, reflectDir,1);
                half3 sky = DecodeHDREnvironment(rgbm, unity_SpecCube0_HDR);

				//Caustics
				float3 CausticsWorldPos = GetWorldPosFromDepth(Input.positionSS,Input.positionWS);
				CausticsWorldPos.xz = 0.1*CausticsWorldPos.xz*_CausticsTexture_ST.xy+_CausticsTexture_ST.zw;
				float2 causticsDistortion = normal.xz*_CausticsDistort;
				float2 causticsUV = CausticsWorldPos.xz+causticsDistortion;
				causticsUV+= _Time.y*_CausticsSpeed.xy;

				float cauticsRange = 1-saturate((eyeDepth-Input.positionSS.w)/_CauticsRange)*1;
				float4 causticsMap = SAMPLE_TEXTURE2D(_CausticsTexture,sampler_CausticsTexture,causticsUV)*cauticsRange;//saturate(1-depth*_CauticsRange);
				causticsMap*=_CausticsTint;
				cameraColorTex = cameraColorTex+cameraColorTex*causticsMap;

				
				// Get Baked GI
				half3 bakedGI = SAMPLE_GI(Input.lightmapUV, Input.vertexSH, normal);
				
				// Main Light & Shadows
				float4 shadowCoord = TransformWorldToShadowCoord(Input.positionWS.xyz);
				Light mainLight = GetMainLight(shadowCoord);
				//mainLight.shadowAttenuation = 1;
				half3 attenuatedLightColor = mainLight.color * (mainLight.distanceAttenuation * mainLight.shadowAttenuation);
				attenuatedLightColor = lerp(attenuatedLightColor,mainLight.color,1-_ShadowStrength);
				
				// Mix Realtime & Baked (if LIGHTMAP_SHADOW_MIXING / _MIXED_LIGHTING_SUBTRACTIVE is enabled)
				MixRealtimeAndBakedGI(mainLight, Input.normalWS, bakedGI);

				// Diffuse
				half3 shading = bakedGI + 0.5*LightingLambert(attenuatedLightColor, mainLight.direction, normal)+1;
				half4 color = lerp(cameraColorTex,colormap,depth);
				
				//specular
				float NdotH = saturate(dot(normal,normalize(mainLight.direction+GetCameraPositionWS()-normal)));
				NdotH = pow(NdotH,4)*(1-depth);
				//float4 foamColor= lerp(colormap,_FoamColor,foam);
				color.rgb = lerp(color.rgb,_FoamColor.rgb,foam);
				color.rgb = lerp(color.rgb,sky,saturate(depth+_ReflectStrength*2-1));
				// if(Input.positionWS.y>48.0)
				// 	return float4(1,0,0,1);
				return float4(color.rgb*shading,1);
				//return float4(smoothstep(0,0.15,cameraDepth).xxx,1);
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
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Shadows.hlsl"

			// Shadow Casting Light geometric parameters. These variables are used when applying the shadow Normal Bias and are set by UnityEngine.Rendering.Universal.ShadowUtils.SetupShadowCasterConstantBuffer in com.unity.render-pipelines.universal/Runtime/ShadowUtils.cs
			// For Directional lights, _LightDirection is used when applying shadow Normal Bias.
			// For Spot lights and Point lights, _LightPosition is used to compute the actual light direction because it is different at each shadow caster geometry vertex.
			float3 _LightDirection;
			float3 _LightPosition;

			struct Attributes
			{
			    float4 positionOS   : POSITION;
			    float3 normalOS     : NORMAL;
			    float2 texcoord     : TEXCOORD0;
			    UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct Varyings
			{
			    float2 uv           : TEXCOORD0;
			    float4 positionCS   : SV_POSITION;
				float3 normalWS		: TEXCOORD1;
			};

			float4 GetShadowPositionHClip(Attributes input)
			{
			    float3 positionWS = TransformObjectToWorld(input.positionOS.xyz);
			    float3 normalWS = TransformObjectToWorldNormal(input.normalOS);

			#if _CASTING_PUNCTUAL_LIGHT_SHADOW
			    float3 lightDirectionWS = normalize(_LightPosition - positionWS);
			#else
			    float3 lightDirectionWS = _LightDirection;
			#endif

			    float4 positionCS = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));

			#if UNITY_REVERSED_Z
			    positionCS.z = min(positionCS.z, UNITY_NEAR_CLIP_VALUE);
			#else
			    positionCS.z = max(positionCS.z, UNITY_NEAR_CLIP_VALUE);
			#endif

			    return positionCS;
			}

			void GerstnerWave_float (
				    float4 waveDir, float4 waveParam, float3 p, out float3 delta_pos, out float3 delta_normalWS
				) {
				    // waveParam : steepness, waveLength, speed, amplify
				    float steepness = waveParam.x;
				    float wavelength = waveParam.y;
				    float speed = waveParam.z;
				    float amplify = waveParam.w;
				    float2 d = normalize(waveDir.xz);

				    float w = 2 * 3.1415 / wavelength;
				    float f = w * (dot(d, p.xz) - _Time.y * speed);
				    float sinf = sin(f);
				    float cosf = cos(f);

				    steepness = clamp(steepness, 0, 1 / (w*amplify));

				    delta_normalWS = float3(
				        - amplify * w * d.x * cosf,
				        - steepness * amplify * w * sinf,
				        - amplify * w * d.y * cosf
				    );

				    delta_pos = float3(
				        steepness * amplify * d.x * cosf,
				        amplify * sinf,
				        steepness * amplify * d.y * cosf
				    );
				}
			
			Varyings ShadowPassVertex(Attributes input)
			{
			    Varyings output;
			    UNITY_SETUP_INSTANCE_ID(input);

			    output.uv = TRANSFORM_TEX(input.texcoord, _BaseMap);
				float3 worldPos = TransformObjectToWorld(input.positionOS);
				float offsetOS = _WaveAmplitude*sin(_WaveSpeed*_Time.y + worldPos.z*_WaveFrequency);
				float3 normalWS = TransformObjectToWorldNormal(input.normalOS);
				float3 delta_pos , delta_normalWS;
				GerstnerWave_float(float4(0,1,1,1),_ParamsA,worldPos,delta_pos,delta_normalWS);
				input.positionOS.xyz+=delta_pos;
				normalWS+=delta_normalWS;
				GerstnerWave_float(float4(0.2,1,1,1),_ParamsB,worldPos,delta_pos,delta_normalWS);
				input.positionOS.xyz+=delta_pos;
				normalWS+=delta_normalWS;
				GerstnerWave_float(float4(-0.8,1,1,1),_ParamsC,worldPos,delta_pos,delta_normalWS);
				input.positionOS.xyz+=delta_pos;
				normalWS+=delta_normalWS;
				//normalWS += delta_normalWS;
				// GerstnerWave_float(float4(1,0,1,1),float4(0.5,5,0.2,0.1),worldPos,delta_pos,delta_normalWS);
				// input.positionOS.xyz+=delta_pos;
				//input.positionOS.y+=offsetOS;
			    output.positionCS = GetShadowPositionHClip(input);
				output.normalWS = normalWS;
			    return output;
			}

			half4 ShadowPassFragment(Varyings input) : SV_TARGET
			{
			    Alpha(SampleAlbedoAlpha(input.uv, TEXTURE2D_ARGS(_BaseMap, sampler_BaseMap)).a, _BaseColor, _Cutoff);
			    return 0;
			}
			
			ENDHLSL
		}

		
		
	}
}