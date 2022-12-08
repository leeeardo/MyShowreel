Shader "Custom/Snow Interactive" {
    Properties{
        [Header(Main)]
    	_MainTex("Snow Texture", 2D) = "white" {}
    	_NormalTex("Normal Texture",2D) = "bump"{}
    	_BumpScale("Bump Scale" , float) = 1
    	_OcclusionTex("Occlusion",2D) = "white"{}
    	_OcclusionStrength("OcclusionStrength",Range(0,2))=1
        _RoughnessTex("Roughness",2D) = "white"{}

    	_StepThreshold("Threshold",range(0,1)) = 1
    	    	
        _DisplacementNoise("Snow Displacement Noise", 2D) = "gray" {}
        _NoiseScale("Displacement Scale", Range(0,2)) = 0.1
        _NoiseWeight("Displacement Weight", Range(0,2)) = 0.1
        [HDR]_ShadowColor("Shadow Color", Color) = (0.5,0.5,0.5,1)
        
        [Header(Tesselation)]
        _MaxTessDistance("Max Tessellation Distance", Range(10,100)) = 50
        _Tess("Tessellation", Range(1,32)) = 20
        
        [Header(Snow)]
        //[HDR]_Color("Snow Color", Color) = (0.5,0.5,0.5,1)
        [HDR]_PathColorTint("Snow Path Color In", Color) = (0.5,0.5,0.7,1)
        //[HDR]_PathColorOut("Snow Path Color Out", Color) = (0.5,0.5,0.7,1)
        //_PathBlending("Snow Path Blending", Range(0,3)) = 0.3
        _SnowHeight("Snow Height", Range(0,2)) = 0.3
        _SnowDepth("Snow Path Depth", Range(0,100)) = 0.3
        //_SnowTextureOpacity("Snow Texture Opacity", Range(0,2)) = 0.3
        //_SnowTextureScale("Snow Texture Scale", Range(0,2)) = 0.3
 
//        [Space]
//        [Header(Sparkles)]
//        _SparkleScale("Sparkle Scale", Range(0,10)) = 10
//        _SparkCutoff("Sparkle Cutoff", Range(0,10)) = 0.8
//        _SparkleNoise("Sparkle Noise", 2D) = "gray" {}
 
//        [Space]
//        [Header(Rim)]
//        _RimPower("Rim Power", Range(0,20)) = 20
//        [HDR]_RimColor("Rim Color Snow", Color) = (0.5,0.5,0.5,1)
    }
    HLSLINCLUDE
 
    
    ENDHLSL
 
    SubShader{
        Tags{ "RenderType" = "Opaque" "RenderPipeline" = "UniversalPipeline"}
 
        Pass{
            Tags { "LightMode" = "UniversalForward" }
 
            HLSLPROGRAM
            // vertex happens in snowtessellation.hlsl
            
            // Includes
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "DistanceBasedTessellation.hlsl"
            
            #pragma require tessellation tessHW
            #pragma vertex SnowVert
           
            #pragma fragment SnowFrag
            #pragma target 4.0
            // Keywords
           
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
            #pragma multi_compile _ _SHADOWS_SOFT
            #pragma multi_compile_fog
            
            float4 _Color, _RimColor;
            float _RimPower;
            float4 _PathColorTint, _PathColorOut;
            float _PathBlending;
            float _SparkleScale, _SparkCutoff;
            float _SnowTextureOpacity, _SnowTextureScale;
            float4 _ShadowColor;
            #if UNITY_CAN_COMPILE_TESSELLATION
            ControlPoint SnowVert(Attributes input)
            {
                ControlPoint output;
                output.positionOS = input.positionOS;
                output.uv = input.uv;
                output.normalOS = input.normalOS;
            	output.tangentOS = input.tangentOS;

                output.LightmapUV = input.LightmapUV;
                return output;
            }
            #else
            Varyings SnowVert(Attributes input)
            {
                Varyings output;
                output.positionCS = TransformObjectToHClip(input.positionOS.xyz);
                output.uv = input.uv;
                output.normalWS = TransformObjectToWorldNormal(input.normalOS);
                output.positionWS = TransformObjectToWorld(input.positionOS.xyz);
                return output;
            }
            #endif
            
			TEXTURE2D(_NormalTex);
            SAMPLER(sampler_NormalTex);
            TEXTURE2D(_OcclusionTex);
            SAMPLER(sampler_OcclusionTex);
            TEXTURE2D(_RoughnessTex);
            SAMPLER(sampler_RoughnessTex);

            TEXTURE2D(_TraceHoleRamp);
            SAMPLER(sampler_TraceHoleRamp);

            float _OcclusionStrength,_BumpScale;
     

            
            half4 SnowFrag(Varyings input) : SV_Target{

                float3 worldPos = input.positionWS.xyz;
                float2 rtUV = worldPos.xz -_Position.xz;
                rtUV = rtUV/(_OrthographicCamSize *2);
                rtUV +=0.5;
                float effectRT = SAMPLE_TEXTURE2D(_GlobalEffectRT,sampler_GlobalEffectRT,rtUV).g;
                 effectRT *=  smoothstep(0.99, 0.9, rtUV.x) * smoothstep(0.99, 0.9,1- rtUV.x);
                 effectRT *=  smoothstep(0.99, 0.9, rtUV.y) * smoothstep(0.99, 0.9,1- rtUV.y);
            	float traceMask = saturate(smoothstep(0.0,_StepThreshold,effectRT));

                //float snowNoise = SAMPLE_TEXTURE2D_LOD(_Noise,sampler_Noise,worldPos.xz*_NoiseScale,0).r;

            	//albedo
            	half4 baseMap = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, input.uv);

            	//normal
            	float3 tangentWS = normalize(input.tangentWS.xyz);
            	float3 normalWS = normalize(input.normalWS.xyz);
            	float3 binormalWS = normalize(cross(tangentWS,normalWS)*input.tangentWS.w);
            	float3x3 TBN = float3x3(tangentWS,binormalWS,normalWS);
            	float3 normalTex = UnpackNormalScale( SAMPLE_TEXTURE2D(_NormalTex, sampler_NormalTex, input.uv),_BumpScale);
            	float3 normal = normalize( mul(normalTex,TBN));

            	//ao metallic roughness
            	float aoMask = SAMPLE_TEXTURE2D(_OcclusionTex, sampler_OcclusionTex, input.uv);
            	aoMask = aoMask+_OcclusionStrength;
            	float metallic = 0;
            	float roughness = SAMPLE_TEXTURE2D(_OcclusionTex, sampler_OcclusionTex, input.uv);
            	
				// Get Baked GI
				half3 bakedGI = SAMPLE_GI(Input.lightmapUV, input.vertexSH, input.normalWS);
				
				// Main Light & Shadows
				float4 shadowCoord = TransformWorldToShadowCoord(input.positionWS.xyz);
				Light mainLight = GetMainLight(shadowCoord);
				half3 attenuatedLightColor = mainLight.color * (mainLight.distanceAttenuation * mainLight.shadowAttenuation);

				// Mix Realtime & Baked (if LIGHTMAP_SHADOW_MIXING / _MIXED_LIGHTING_SUBTRACTIVE is enabled)
				MixRealtimeAndBakedGI(mainLight, input.normalWS, bakedGI);

				// Diffuse
				half3 shading = bakedGI + LightingLambert(attenuatedLightColor, mainLight.direction, normal);
				half4 notMaskColor = baseMap*(1-effectRT)*aoMask;

				//pbr try
            	InputData input_data;
            	input_data.positionWS = input.positionWS;
            	input_data.tangentToWorld = TBN;
            	input_data.normalWS = NormalizeNormalPerPixel(normal);
            	input_data.viewDirectionWS = GetWorldSpaceViewDir(input.positionWS);
            	input_data.shadowCoord = shadowCoord;
            	input_data.fogCoord = 0;
            	input_data.bakedGI = bakedGI;
            	input_data.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(input.positionCS);
				input_data.shadowMask = SAMPLE_SHADOWMASK(input.LightmapUV);
				float4 pbrColor = UniversalFragmentPBR(input_data,baseMap,metallic,0,1-roughness,0,0,1);

				//holeRampsample
            	//float holemask = smoothstep(0.0,0.5,traceMask);
            	//float4 stepColor = lerp(_PathColorIn,_PathColorOut,traceMask)*effectRT;

            	float3 color = pbrColor*(1-traceMask)+pbrColor*_PathColorTint.rgb*traceMask;

            	//extraLight
            	float3 extraLight;
            	int pixelLightCount = GetAdditionalLightsCount();
            	for (int j=0;j<pixelLightCount;j++)
            	{
            		Light light = GetAdditionalLight(j,input.positionWS,input_data.shadowMask);
            		float3 attenuatedLightColor = light.color*(light.distanceAttenuation*light.shadowAttenuation);
            		extraLight += attenuatedLightColor;
            	}
            	extraLight*=baseMap.rgb;
            	extraLight*=aoMask;
				return float4(extraLight+color,1);
            }
            ENDHLSL
 
        }

    }
}