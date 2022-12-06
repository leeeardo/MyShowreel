Shader "Customs/NotTerrainShader"
{
    Properties
    {
        _mainTex("SoilTexture",2D)="white"{}
        _mainTex2("grassTexture",2D)="white"{}
    }

    SubShader
    {
         Tags { "RenderPipeline"="UniversalRenderPipeline" "RenderType"="Opaque" "Queue"= "Geometry" }

        HLSLINCLUDE 
        
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/lighting.hlsl"

        CBUFFER_START(UnityPerMaterial)
        TEXTURE2D(_mainTex);
        SAMPLER(sampler_mainTex);
        float4 _mainTex_ST;
        TEXTURE2D(_mainTex2);
        SAMPLER(sampler_mainTex2);
        float4 _mainTex2_ST;
        CBUFFER_END

        struct a2v
        {
            float4 positionOS : POSITION;
            float3 normalOS : NORMAL;
            float4 tangentOS : TANGENT;
            float2 uv : TEXCOORD0;
            float3 color : COLOR;
        };

        struct v2f
        {
            float4 positionCS : SV_POSITION;
            float4 uv : TEXCOORD0;
            float3 color : COLOR;
        };

        ENDHLSL

        Pass
        {   
            Name"MyTerrain"
            Tags{"LightMode" = "UniversalForward"}
            
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            v2f vert (a2v input)
            {
                v2f output;
                VertexPositionInputs i = (VertexPositionInputs)0;
                i = GetVertexPositionInputs(input.positionOS.xyz);
                output.positionCS = i.positionCS;
                output.uv.xy = TRANSFORM_TEX(input.uv,_mainTex);
                output.uv.zw = TRANSFORM_TEX(input.uv,_mainTex2);
                output.color = input.color;
                return output;
            }

            float4 frag (v2f input) : SV_Target
            {
                float3 soil = SAMPLE_TEXTURE2D(_mainTex,sampler_mainTex,input.uv.xy).rgb;
                float3 grass = SAMPLE_TEXTURE2D(_mainTex2,sampler_mainTex2,input.uv.zw).rgb;
                float3 color = soil * input.color.r + grass * input.color.g;
                
                return float4(soil,1);
            }
            ENDHLSL
        }
        
    }
}
