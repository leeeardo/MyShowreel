#ifndef _DISTANCE_BASED_TESSELLATION
#define _DISTANCE_BASED_TESSELLATION

#if defined(SHADER_API_D3D11) || defined(SHADER_API_GLES3) || defined(SHADER_API_GLCORE) || defined(SHADER_API_VULKAN) || defined(SHADER_API_METAL) || defined(SHADER_API_PSSL)
#define UNITY_CAN_COMPILE_TESSELLATION 1
#   define UNITY_domain                 domain
#   define UNITY_partitioning           partitioning
#   define UNITY_outputtopology         outputtopology
#   define UNITY_patchconstantfunc      patchconstantfunc
#   define UNITY_outputcontrolpoints    outputcontrolpoints
#endif

#pragma hull DistanceBasedTessellationHull
#pragma domain DistanceBasedTessellationDomain

struct Attributes
{
    float4 positionOS : POSITION;
    float3 normalOS : NORMAL;
    float4 tangentOS : TANGENT;
    float2 uv : TEXCOORD0;
    float2 LightmapUV   : TEXCOORD1;
};
struct ControlPoint
{
    float4 positionOS : INTERNALTESSPOS;
    float2 uv : TEXCOORD0;
    float3 normalOS : NORMAL;
    float4 tangentOS : TANGENT;
    float2 LightmapUV   : TEXCOORD1;

};

struct TessllationFactors
{
    float edge[3] : SV_TessFactor;
    float inside : SV_InsideTessFactor;
};
struct Varyings
{
    float4 positionCS : SV_POSITION;
    float2 uv : TEXCOORD0;
    float3 positionWS : TEXCOORD1;
    float3 normalWS : NORMAL;
    float4 tangentWS : TANGENT;
    float fogFactor : TEXCOORD2;
    DECLARE_LIGHTMAP_OR_SH(LightmapUV, vertexSH, 3);
};
[UNITY_domain("tri")]
[UNITY_outputcontrolpoints(3)]
[UNITY_outputtopology("triangle_cw")]
[UNITY_partitioning("fractional_odd")]
[UNITY_patchconstantfunc("patchConstantFunction")]
ControlPoint DistanceBasedTessellationHull(InputPatch<ControlPoint, 3> patch, uint id : SV_OutputControlPointID)
{
    return patch[id];
}
TessllationFactors CalcTriEdgeTessFactors(float3 triVertFactors)
{
    TessllationFactors tessFactors;
    tessFactors.edge[0] = 0.5*(triVertFactors.y+triVertFactors.z);
    tessFactors.edge[1] = 0.5*(triVertFactors.x+triVertFactors.z);
    tessFactors.edge[2] = 0.5*(triVertFactors.y+triVertFactors.x);
    tessFactors.inside = (triVertFactors.x+triVertFactors.y+triVertFactors.z)/3;
    return tessFactors;
}
float3 _CharacterPos;
float CalDistanceTessFactor(float4 v0,float minDist,float maxDist, float tess)
{
    float3 worldPos = TransformObjectToWorld(v0.xyz);
    float3 camPos = _CharacterPos;//GetCameraPositionWS();
    float dist = distance(worldPos,camPos);
    float f = clamp((maxDist-dist)/(maxDist-minDist),0.01,1.0);
    return f*tess;
}

TessllationFactors DistanceBasedTess(float4 v0,float4 v1,float4 v2,float minDist,float maxDist, float tess)
{
    float3 f;
    f.x = CalDistanceTessFactor(v0,minDist,maxDist,tess);
    f.y = CalDistanceTessFactor(v1,minDist,maxDist,tess);
    f.z = CalDistanceTessFactor(v2,minDist,maxDist,tess);
    return CalcTriEdgeTessFactors(f);
}

float _Tess;
float _MaxTessDistance;
TessllationFactors patchConstantFunction(InputPatch<ControlPoint, 3> patch)
{
    float minDist = 2.0;
    float maxDist = _MaxTessDistance;
    return DistanceBasedTess(
        patch[0].positionOS,patch[1].positionOS,patch[2].positionOS,minDist,maxDist,_Tess);
}

float _OrthographicCamSize,_NoiseScale;
float3 _Position;
TEXTURE2D(_MainTex);
SAMPLER(sampler_MainTex);
float4 _MainTex_ST;

TEXTURE2D(_SnowTrace);
SAMPLER(sampler_SnowTrace);
TEXTURE2D(_DisplacementNoise);
SAMPLER(sampler_DisplacementNoise);
float _PathBlending;
float _SnowHeight,_NoiseWeight,_SnowDepth;
float test,_StepThreshold;
Varyings vert(Attributes input)
{
    Varyings output = (Varyings)0;
    
    //处理路径遮罩
    float3 worldPos = TransformObjectToWorld(input.positionOS.xyz);
    float2 rtUV = worldPos.xz -_Position.xz;
    rtUV = rtUV/(_OrthographicCamSize *2);
    rtUV +=0.5;
    //获取rt
    float4 effectRT = SAMPLE_TEXTURE2D_LOD(_SnowTrace,sampler_SnowTrace,rtUV,0);
    //effectRT = saturate(smoothstep(0.0,_StepThreshold,effectRT));
    
    //smoothstep防出血？
    // mask to prevent bleeding
    // effectRT *=  smoothstep(0.99, 0.9, rtUV.x) * smoothstep(0.99, 0.9,1- rtUV.x);
    // effectRT *=  smoothstep(0.99, 0.9, rtUV.y) * smoothstep(0.99, 0.9,1- rtUV.y);
    float traceMask = saturate(smoothstep(0.0,_PathBlending,effectRT.g));
    float SnowNoise = SAMPLE_TEXTURE2D_LOD(_DisplacementNoise,sampler_DisplacementNoise,worldPos.xz*_NoiseScale,0).r;
    input.positionOS.xyz += SafeNormalize(
    input.normalOS)*saturate((_SnowHeight+SnowNoise*_NoiseWeight))*saturate(1-traceMask*_SnowDepth);
    
    VertexNormalInputs normalInput = GetVertexNormalInputs(input.normalOS,input.tangentOS);

    output.normalWS = normalInput.normalWS;
    output.tangentWS = float4(TransformObjectToWorld(input.tangentOS.xyz),input.tangentOS.w);
    output.positionCS = TransformObjectToHClip(input.positionOS.xyz);
    output.uv = TRANSFORM_TEX(input.uv,_MainTex);

    output.positionWS = worldPos;
    output.fogFactor = ComputeFogFactor(output.positionCS.z);
    OUTPUT_LIGHTMAP_UV(input.lightmapUV, unity_LightmapST, output.LightmapUV);
    OUTPUT_SH(output.normalWS.xyz, output.vertexSH);
    return output;
}

[UNITY_domain("tri")]
Varyings DistanceBasedTessellationDomain(
    TessllationFactors factors,OutputPatch<ControlPoint,3> patch , float3 barycentriCoordinates : SV_DomainLocation)
{
    Attributes v;
    #define Interpolate(fieldName) v.fieldName = \
                patch[0].fieldName * barycentriCoordinates.x + \
                patch[1].fieldName * barycentriCoordinates.y + \
                patch[2].fieldName * barycentriCoordinates.z;

    Interpolate(positionOS)
    Interpolate(uv)
    Interpolate(normalOS)
    Interpolate(tangentOS)
    Interpolate(LightmapUV)

    return vert(v);
}           
#endif