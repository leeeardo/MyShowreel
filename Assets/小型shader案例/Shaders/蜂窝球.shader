Shader "Customs/EnergySphere(NoDoneYet)"
{
    Properties
    {
        _MainTex ("_MainTex", 2D) = "white" {}
        _Mask("Mask" , 2D) = "white"{}
        _NormalScale("Normal Scale" , Range(0,1)) = 1
        _MainColor("Color" , Color) = (1,1,1,1)
        _Amount("Amount" , float) = 100
        _DepthOffset("DepthOffset" , Range(0.9,0.98)) = 1
        _DepthLeft("DepthLeft" , Range(0,1)) = 0
        _DepthRight("DepthRight" , Range(0,1)) = 1
        _UpMoveLeft("_UpMoveLeft" , Range(0,1)) = 0
        _UpMoveRight("_UpMoveRight" , Range(0,1)) = 1
        _UpMoveSpeed("Up Move Speed" , Range(0,5)) = 0.5
        _FresnelStrenght("Fresnel" , float) = 1
        [Toggle(_PARALLAX_ON)] _ParallaxToggle ("IfParallax", Float) = 0
        _ParallaxHeight("ParallaxHeight" , Range(-1,1)) = 1
        [HDR]_EmissionColor("Emission Color" , Color) = (1,1,1,1)
    }
    SubShader
    {
        Tags { "RenderPipeline"="UniversalRenderPipeline" "RenderType"="Transparent" "Queue"= "Transparent" }

        HLSLINCLUDE
        
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/lighting.hlsl"

        #pragma shader_feature _PARALLAX_ON

        CBUFFER_START(UnityPerMaterial)
        float4 _MainTex_ST;
        float4 _MainColor;
        float _NormalScale;
        float _ParallaxHeight;
        float _DepthOffset;
        float _DepthLeft;
        float _DepthRight;
        float _FresnelStrenght;
        float _UpMoveLeft;
        float _UpMoveRight;
        float4 _EmissionColor;
        float _UpMoveSpeed;
        CBUFFER_END
        float4 _CameraDepthTexture_TexelSize;
        TEXTURE2D(_MainTex);
        TEXTURE2D(_Mask);
        SAMPLER(sampler_MainTex);
        SAMPLER(sampler_Mask);
        SAMPLER(_CameraDepthTexture);
        float _Amount;
        struct a2v
        {
            float4 positionOS : POSITION;
            float3 normalOS : NORMAL;
            float4 tangentOS : TANGENT;
            float2 uv : TEXCOORD0;
        };

        struct v2f
        {
            float4 positionCS : SV_POSITION;
            float2 uv : TEXCOORD0;
            float3 normalWS : NORMAL;
            float4 tangentWS : TANGENT;
            float3 positionWS : TEXCOORD1;
            float4 positionSS : TEXCOORD2;
            float3 viewDirWS : TEXCOORD3;
        };

        ENDHLSL


        Pass
        {   
            Name"TestTC"
            Tags{"LightMode" = "UniversalForward"}
            Blend SrcAlpha OneMinusSrcAlpha
            Cull Back
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            v2f vert (a2v v)
            {
                v2f o;
                VertexPositionInputs input = (VertexPositionInputs)0;
                input = GetVertexPositionInputs(v.positionOS);
                o.positionCS = input.positionCS;
                o.uv = v.uv;
                o.positionWS = input.positionWS;
                o.positionSS = input.positionNDC;
                o.normalWS = TransformObjectToWorldNormal(v.normalOS);
                o.tangentWS.xyz = TransformObjectToWorldDir(v.tangentOS);
                o.tangentWS.w= v.tangentOS.w;
                o.viewDirWS = GetWorldSpaceViewDir(input.positionWS);
                return o;
            }

            void DoParallax(inout v2f i)
            {
                #ifdef _PARALLAX_ON
                    float3 tangent = normalize(i.tangentWS).xyz;
                    float3 normal = normalize(i.normalWS).xyz;
                    float3 bitangent = cross(normal,tangent)*i.tangentWS.w;
                    float3x3 TBN = float3x3(tangent,bitangent,normal);
                    float3 viewDirTS = normalize(mul(TBN,i.viewDirWS));
                    viewDirTS.xy /= (viewDirTS.z+0.42);
                    float height = _ParallaxHeight;
                    height *= 0.5;
                    float2 uvOffset = viewDirTS * height;
                    i.uv.xy += uvOffset;
                #endif
            }

            float4 frag (v2f i) : SV_Target
            {
                DoParallax(i);
                float2 screenUV = i.positionSS.xy/i.positionSS.w;
                //GetDepthTexture
                float4 depthColor = tex2D(_CameraDepthTexture,screenUV).r;
                float depthBuffer = Linear01Depth(depthColor,_ZBufferParams);
                
                //GetFragDepth
                float fragDepth = i.positionCS.z;
                fragDepth = Linear01Depth(fragDepth,_ZBufferParams);
                float edge= (fragDepth-depthBuffer+0.005)*100*_DepthOffset;
                 edge = saturate(smoothstep(_DepthLeft,_DepthRight,edge));

                //Fresnel
                float NdotV = abs(dot(normalize(i.normalWS),normalize(i.viewDirWS)));
                //NdotV = NdotV<0 ? 1:NdotV;
                float fresnel = pow(1-NdotV,_FresnelStrenght);
                //fresnel = fresnel >1 ? 0:fresnel;
                
                //Flowɨ��
                float upMove = abs(frac(i.positionWS.y-_Time.y*_UpMoveSpeed)-0.5);
                upMove = smoothstep(_UpMoveLeft,_UpMoveRight,upMove);
                

                float3 baseMap = SAMPLE_TEXTURE2D(_MainTex,sampler_MainTex,i.uv).rgb;
                float mask = SAMPLE_TEXTURE2D(_Mask,sampler_Mask,i.uv).b;
                upMove*=baseMap.r;
                float3 flow = _EmissionColor * baseMap.r;
                //return upMove;
                //edge = pow(edge , 10);
                //return upMove;
                return float4(_EmissionColor.rgb,upMove*mask+fresnel+edge);
            }
            ENDHLSL
        }
        Pass
        {   
            Name"2dd"
            Tags{"LightMode" = "SRPDefaultUnlit"}
            Blend SrcAlpha OneMinusSrcAlpha
            Cull Front
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            v2f vert (a2v v)
            {
                v2f o;
                VertexPositionInputs input = (VertexPositionInputs)0;
                input = GetVertexPositionInputs(v.positionOS);
                o.positionCS = input.positionCS;
                o.uv =v.uv;
                o.positionWS = input.positionWS;
                o.positionSS = input.positionNDC;
                o.normalWS = TransformObjectToWorldNormal(v.normalOS);
                o.tangentWS.xyz = TransformObjectToWorldDir(v.tangentOS);
                o.tangentWS.w= v.tangentOS.w;
                o.viewDirWS = GetWorldSpaceViewDir(input.positionWS);
                return o;
            }

            void DoParallax(inout v2f i)
            {
                #ifdef _PARALLAX_ON
                    float3 tangent = normalize(i.tangentWS).xyz;
                    float3 normal = normalize(i.normalWS).xyz;
                    float3 bitangent = cross(normal,tangent)*i.tangentWS.w;
                    float3x3 TBN = float3x3(tangent,bitangent,normal);
                    float3 viewDirTS = normalize(mul(TBN,i.viewDirWS));
                    viewDirTS.xy /= (viewDirTS.z+0.42);
                    float height = _ParallaxHeight;
                    height *= 0.5;
                    float2 uvOffset = viewDirTS * height;
                    i.uv.xy += uvOffset;
                #endif
            }

            float4 frag (v2f i) : SV_Target
            {
                DoParallax(i);
                float2 screenUV = i.positionSS.xy/i.positionSS.w;
                //GetDepthTexture
                float4 depthColor = tex2D(_CameraDepthTexture,screenUV).r;
                float depthBuffer = Linear01Depth(depthColor,_ZBufferParams);
                
                //GetFragDepth
                float fragDepth = i.positionCS.z;
                fragDepth = Linear01Depth(fragDepth,_ZBufferParams);
                float edge= (fragDepth-depthBuffer+0.005)*100*_DepthOffset;
                 edge = saturate(smoothstep(_DepthLeft,_DepthRight,edge));

                //Fresnel
                float NdotV = abs(dot(normalize(i.normalWS),normalize(i.viewDirWS)));
                //NdotV = NdotV<0 ? 1:NdotV;
                float fresnel = pow(1-NdotV,_FresnelStrenght);
                //fresnel = fresnel >1 ? 0:fresnel;
                
                //Flowɨ��
                float upMove = abs(frac(i.positionWS.y-_Time.y*_UpMoveSpeed)-0.5);
                upMove = smoothstep(_UpMoveLeft,_UpMoveRight,upMove);
                

                float3 baseMap = SAMPLE_TEXTURE2D(_MainTex,sampler_MainTex,i.uv).rgb;
                float mask = SAMPLE_TEXTURE2D(_Mask,sampler_Mask,i.uv).b;
                upMove*=baseMap.r;
                float3 flow = _EmissionColor * baseMap.r;
                //return upMove;
                //edge = pow(edge , 10);
                //return upMove;
                return float4(_EmissionColor.rgb,(upMove)*mask+fresnel+edge);
            }
            ENDHLSL
        }
    }
}
