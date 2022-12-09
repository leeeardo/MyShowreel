Shader "Custom/Stylized Snow Interactive" {
    Properties{
        [Header(Main)]
    	_MainTex("Snow Texture", 2D) = "white" {}
    	[HDR]_Color("Snow Color", Color) = (0.5,0.5,0.5,1)
    	_SnowTextureOpacity("Snow Texture Opacity", Range(0,2)) = 0.3
		[HDR]_ShadowColor("Shadow Color", Color) = (0.5,0.5,0.5,1)
    	//_PathBlend("Threshold",range(0,1)) = 1
    	    	
        _DisplacementNoise("Snow Displacement Noise", 2D) = "gray" {}
        _NoiseScale("Displacement Scale", Range(0,2)) = 0.1
        _NoiseWeight("Displacement Weight", Range(0,2)) = 0.1
        [HDR]_ShadowColor("Shadow Color", Color) = (0.5,0.5,0.5,1)
        
        [Header(Tesselation)]
        _MaxTessDistance("Max Tessellation Distance", Range(10,100)) = 50
        _Tess("Tessellation", Range(1,32)) = 20
        
        [Header(Snow)]
        [HDR]_PathColorIn("Snow Path Color In", Color) = (0.5,0.5,0.7,1)
        [HDR]_PathColorOut("Snow Path Color Out", Color) = (0.5,0.5,0.7,1)
    	_PathBlending("Path Blending", Range(0,1)) = 1
        _PathColorBlending("Snow Color Path Blending", Range(0,3)) = 0.3
        _SnowHeight("Snow Height", Range(0,2)) = 0.3
        _SnowDepth("Snow Path Depth", Range(0,100)) = 0.3
 
        [Space]
        [Header(Sparkles)]
        _SparkleScale("Sparkle Scale", Range(0,10)) = 10
        _SparkCutoff("Sparkle Cutoff", Range(0,10)) = 0.8
        _SparkleNoise("Sparkle Noise", 2D) = "gray" {}
 
        [Space]
        [Header(Rim)]
        _RimPower("Rim Power", Range(0,20)) = 20
        [HDR]_RimColor("Rim Color Snow", Color) = (0.5,0.5,0.5,1)
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
            float4 _PathColorIn, _PathColorOut;
            float _PathColorBlending;
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

            TEXTURE2D(_TraceHoleRamp);
            SAMPLER(sampler_TraceHoleRamp);
			TEXTURE2D(_SparkleNoise);
            SAMPLER(sampler_SparkleNoise);
            float _OcclusionStrength,_BumpScale;
 
            
            half4 SnowFrag(Varyings input) : SV_Target{

                float3 worldPos = input.positionWS.xyz;
                float2 rtUV = worldPos.xz -_Position.xz;
                rtUV = rtUV/(_OrthographicCamSize *2);
                rtUV +=0.5;
                float effectRT = SAMPLE_TEXTURE2D(_GlobalEffectRT,sampler_GlobalEffectRT,rtUV).g;
                 effectRT *=  smoothstep(0.99, 0.9, rtUV.x) * smoothstep(0.99, 0.9,1- rtUV.x);
                 effectRT *=  smoothstep(0.99, 0.9, rtUV.y) * smoothstep(0.99, 0.9,1- rtUV.y);
            	float traceMask = saturate(smoothstep(0.0,_PathBlending,effectRT));

                //float snowNoise = SAMPLE_TEXTURE2D_LOD(_Noise,sampler_Noise,worldPos.xz*_NoiseScale,0).r;


            	half4 baseMap = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, input.uv);
            	float3 baseColor = lerp(_Color.rgb,baseMap.rgb,_SnowTextureOpacity);
            	float3 path = lerp(_PathColorIn.rgb,_PathColorOut.rgb,saturate(traceMask*_PathColorBlending));
				//float3 path = traceMask*_PathColorIn.rgb;
            	float3 mainColor = lerp(baseColor,path,saturate(traceMask*_PathColorBlending));
            	
				// Get Baked GI
				//half3 bakedGI = SAMPLE_GI(input.lightmapUV, input.vertexSH, input.normalWS);
				
				// Main Light & Shadows
				float4 shadowCoord = TransformWorldToShadowCoord(input.positionWS.xyz);
				Light mainLight = GetMainLight(shadowCoord);
				half3 mainShadowMask = mainLight.color * (mainLight.distanceAttenuation * mainLight.shadowAttenuation);
				float3 coloredShadow =  _ShadowColor.rgb*(1-mainShadowMask);
				

            	//extraLight
            	float3 extraLight;
            	int pixelLightCount = GetAdditionalLightsCount();
            	for (int j=0;j<pixelLightCount;j++)
            	{
            		Light light = GetAdditionalLight(j,input.positionWS,float4(1,1,1,1));
            		float3 attenuated = light.color*(light.distanceAttenuation*light.shadowAttenuation);
            		extraLight += attenuated;
            	}
            	extraLight*=baseMap.rgb;

            	//sparkle
            	float sparklesStatic = SAMPLE_TEXTURE2D(_SparkleNoise, sampler_SparkleNoise,input.positionWS.xz * _SparkleScale).r;
                float cutoffSparkles = step(_SparkCutoff,sparklesStatic);				
                mainColor += cutoffSparkles  *saturate(1- (traceMask ))*9;
            	//rimlight
            	 // add rim light
            	float3 viewDir = normalize(worldPos-GetCameraPositionWS());
            	float SnowNoise = SAMPLE_TEXTURE2D(_DisplacementNoise,sampler_DisplacementNoise,worldPos.xz*_NoiseScale).r;

                half rim = 1.0 - dot(viewDir,input.normalWS )* SnowNoise.r;
                mainColor += _RimColor.rgb * pow(abs(rim), _RimPower);
            	
            	float3 finalColor = (mainColor+extraLight)*mainShadowMask+coloredShadow;
				return float4(finalColor,1);//float4((mainColor+extraLight)*mainShadowMask,1);
            }
            ENDHLSL
        }
        // Shadow Casting Pass
        Pass
        {
            Name "ShadowCaster"
            Tags{"LightMode" = "ShadowCaster"}

            ZWrite On
            ZTest LEqual
            ColorMask 0
            Cull[_Cull]

            HLSLPROGRAM
            #pragma only_renderers gles gles3 glcore d3d11
            #pragma target 2.0

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing

            // -------------------------------------
            // Material Keywords
            #pragma shader_feature_local_fragment _ALPHATEST_ON
            #pragma shader_feature_local_fragment _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

            // -------------------------------------
            // Universal Pipeline keywords

            // This is used during shadow map generation to differentiate between directional and punctual light shadows, as they use different formulas to apply Normal Bias
            #pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW

            #pragma vertex ShadowPassVertex
            #pragma fragment ShadowPassFragment

            #include "Packages/com.unity.render-pipelines.universal/Shaders/LitInput.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/Shaders/ShadowCasterPass.hlsl"
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags{"LightMode" = "DepthOnly"}

            ZWrite On
            ColorMask 0
            Cull[_Cull]

            HLSLPROGRAM
            #pragma only_renderers gles gles3 glcore d3d11
            #pragma target 2.0

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing

            #pragma vertex DepthOnlyVertex
            #pragma fragment DepthOnlyFragment

            // -------------------------------------
            // Material Keywords
            #pragma shader_feature_local_fragment _ALPHATEST_ON
            #pragma shader_feature_local_fragment _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

            #include "Packages/com.unity.render-pipelines.universal/Shaders/LitInput.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/Shaders/DepthOnlyPass.hlsl"
            ENDHLSL
        }

        // This pass is used when drawing to a _CameraNormalsTexture texture
        Pass
        {
            Name "DepthNormals"
            Tags{"LightMode" = "DepthNormals"}

            ZWrite On
            Cull[_Cull]

            HLSLPROGRAM
            #pragma only_renderers gles gles3 glcore d3d11
            #pragma target 2.0

            #pragma vertex DepthNormalsVertex
            #pragma fragment DepthNormalsFragment

            // -------------------------------------
            // Material Keywords
            #pragma shader_feature_local _NORMALMAP
            #pragma shader_feature_local _PARALLAXMAP
            #pragma shader_feature_local _ _DETAIL_MULX2 _DETAIL_SCALED
            #pragma shader_feature_local_fragment _ALPHATEST_ON
            #pragma shader_feature_local_fragment _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing

            #include "Packages/com.unity.render-pipelines.universal/Shaders/LitInput.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/Shaders/LitDepthNormalsPass.hlsl"
            ENDHLSL
        }

    }
}