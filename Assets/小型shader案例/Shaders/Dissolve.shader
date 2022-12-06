Shader "Customs/Dissolve"
{
    Properties
    {
        [MainTexture] _BaseMap("Texture", 2D) = "white" {}
        [HDR] _BaseColor("Color", Color) = (1, 1, 1, 1)
        _DissolveMask("Mask" , 2D) = "white"{}
        _Dissolve("Dissolve" , Range(0,1)) = 1
        _DissolveEdge("DissolveEdge" , Range(0,1)) = 0.5
        _RampTex("RampTex" , 2D) = "white"{}
        
    }

    SubShader
    {
         Tags { "RenderPipeline"="UniversalRenderPipeline" "RenderType"="Transparent" "Queue"= "Transparent" }

        HLSLINCLUDE
        
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/lighting.hlsl"

        CBUFFER_START(UnityPerMaterial)
        float4 _BaseMap_ST;
        float4 _BaseColor;
        float4 _DissolveMask_ST;
        TEXTURE2D(_BaseMap);
        TEXTURE2D(_DissolveMask);
        TEXTURE2D(_RampTex);
        SAMPLER(sampler_DissolveMask);
        SAMPLER(sampler_BaseMap);
        SAMPLER(sampler_RampTex);
        float _Dissolve;
        float _DissolveEdge;
        CBUFFER_END

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
        };

        ENDHLSL

        Pass
        {   
            Name"Dissolve"
            Tags{"LightMode" = "UniversalForward"}
            Blend SrcAlpha OneMinusSrcAlpha
            Cull Back
            ZWrite Off
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            v2f vert (a2v v)
            {
                v2f o;
                VertexPositionInputs input = (VertexPositionInputs)0;
                input = GetVertexPositionInputs(v.positionOS.xyz);
                o.positionCS = input.positionCS;
                o.uv = TRANSFORM_TEX(v.uv,_BaseMap);
                return o;
            }

            float4 frag (v2f i) : SV_Target
            {
                float dissolve = SAMPLE_TEXTURE2D(_DissolveMask,sampler_DissolveMask,i.uv).r;
                dissolve +=1;
                dissolve -=2* _Dissolve;
                dissolve = smoothstep(0,_DissolveEdge,dissolve);
                float2 newUV = float2(dissolve,0);
                float3 rampTex = SAMPLE_TEXTURE2D(_RampTex,sampler_RampTex,newUV).rgb;
                rampTex*=_BaseColor;
                float3 mainTex = SAMPLE_TEXTURE2D(_BaseMap,sampler_BaseMap,i.uv).rgb;
                float3 color = lerp(rampTex,mainTex,dissolve);
                return float4(color,dissolve);
            }
            ENDHLSL
        }
        
    }

}
