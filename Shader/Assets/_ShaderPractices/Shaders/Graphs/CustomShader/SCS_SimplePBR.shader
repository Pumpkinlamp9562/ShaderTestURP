// Upgrade NOTE: replaced '_Object2World' with 'unity_ObjectToWorld'
// Don't suggest to use it repleatly

Shader "SCS/SimplePBR"
{

    Properties
    {
        [Header(Texture)]
        [Space(5)]
        _MainTex ("Base Map", 2D) = "white" {}
        _ReflectTex ("Reflection Map", CUBE) = "white" {}

        [Header(Settings)]
        [Space(5)]
        _Roughtness("Roughtness",  RANGE(0,1)) = 1
        _RoughtnessThreshold("Roughtness threashold", RANGE(0,1)) = 0.5
        _Metallic("Metallic", RANGE(0,1)) = 1
        _MetallicThreshold("Metallic threashold", RANGE(0,1)) = 0.5
        _Brightness("Brightness", RANGE(0,100)) = 1
        _Threshold("Brightness threashold", RANGE(0,1)) = 0
        _Fresnel("Fresnel", Float) = 1

        [Header(Addtive Settings)]
        [Space(5)]
        [MaterialToggle] _UseLightmap("Use Lightmap?", Float) = 1
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" "RenderPipeline" = "UniversalPipeline"}
        LOD 100

        Pass
        {
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

            struct Attributes
            {
                float3 vertex : POSITION;
                float2 uv : TEXCOORD0;
                float2 lightmapUV : TEXCOORD1;
                float3 normal : NORMAL;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

            struct Varyings
            {
                float2 uv : TEXCOORD0;
                float2 lightmapUV : TEXCOORD1;
                float4 vertex : SV_POSITION;
                float fresnel : TEXCOORD2;
                half3 worldNormal : TEXCOORD3;
                half3 cubeMapUV : TEXCOORD4;
                half3 reflectUV : TEXCOORD5;
                UNITY_VERTEX_INPUT_INSTANCE_ID
                    UNITY_VERTEX_OUTPUT_STEREO
            };

            CBUFFER_START(UnityPerMaterial)
            samplerCUBE _ReflectTex;
            half4 _ReflectTex_HDR;
            sampler2D _MainTex;
            half4 _MainTex_ST;
            half _Roughtness;
            half _Brightness;
            half _Threshold;
            half _RoughtnessThreshold;
            half _MetallicThreshold;
            half _Metallic;
            half _Fresnel;
            half _UseLightmap;

            half4 _Test;

            SamplerState sampler_unity_Lightmap;
            CBUFFER_END

            Varyings vert (Attributes input)
            {
                Varyings output = (Varyings)0;
                output.vertex = TransformObjectToHClip(input.vertex);
                output.uv = TRANSFORM_TEX(input.uv, _MainTex);
                output.lightmapUV = input.lightmapUV.xy * unity_LightmapST.xy + unity_LightmapST.zw;

                output.worldNormal = normalize(TransformObjectToWorldNormal(input.normal));
                half3 worldViewDir = normalize(TransformObjectToWorld(input.vertex) - _WorldSpaceCameraPos.xyz);
                half3 closerNormal = (worldViewDir*-1) - output.worldNormal * _Fresnel ;
                output.fresnel = saturate(dot(output.worldNormal - closerNormal, worldViewDir)) * 0.2;
                // sample the metallice map
                // Transfer the vertex UV coordinates to cube map UV coordinates  _:(´ཀ`」 ∠):
                output.cubeMapUV = float3(-(input.uv.x)+0.5 , (input.uv.y) -0.5 , 0.5);
                output.reflectUV = reflect(-worldViewDir, output.worldNormal);

                return output;
            }
            half4 frag (Varyings i) : SV_Target
            {
                // sample the base map texture
                half4 baseCol = tex2D(_MainTex, i.uv);

                // metalicMask
                half metalicMask = baseCol.a;
                metalicMask = saturate(step(_MetallicThreshold, metalicMask) * (metalicMask - _MetallicThreshold));

                // sample the reflection texture and smoothness
                half rought = abs(texCUBE(_ReflectTex, i.cubeMapUV).a + _Roughtness + 1);
                rought = step(_RoughtnessThreshold, rought) * (rought - _RoughtnessThreshold);
                half4 roughtCol = texCUBElod(_ReflectTex,half4(i.reflectUV, (pow((( rought) * (1 - metalicMask)), (_Roughtness+1) * 2))));
                half3 decodeRoughtCol = DecodeHDREnvironment(roughtCol, _ReflectTex_HDR);

                half4 reflecCol = texCUBElod(_ReflectTex,half4(i.reflectUV, _Roughtness * 8));
                half3 decodeCol = reflecCol * (saturate( baseCol.a + _Roughtness)) * (metalicMask * _Metallic);

                half3 metallicCol = (decodeCol + (decodeRoughtCol* rought));
                
                // sample the lightmap
                half4 decodeInstructions = half4(LIGHTMAP_HDR_MULTIPLIER, LIGHTMAP_HDR_EXPONENT, 0.0h, 0.0h);
                half3 lightCol = DecodeLightmap(SAMPLE_TEXTURE2D(unity_Lightmap, samplerunity_Lightmap, i.lightmapUV), decodeInstructions) * _UseLightmap;

                half4 c = 0;
                c.rgb = (baseCol.rgb * ( (1 - metalicMask) *  (1 - _Metallic)));
                c.rgb += (baseCol.rgb * _Metallic * metallicCol);
                c.rgb += i.fresnel * 0.2 * (1-metalicMask);
                c.rgb += (decodeRoughtCol * rought * (1- _Roughtness) * (1 - _Metallic)) * 0.1;
                c.rgb *= lightCol + (1- _UseLightmap);
                half maxBright = max(max(c.r, c.g), c.b);
                c.rgb *= step(_Threshold, maxBright) * (maxBright - _Threshold) * _Brightness  +1;
                return c;
            }
                ENDHLSL
        }
            //TODO-threashold for all brightness
            //TODO-if use light probe
            //TODO-if use Transparent
            //TODO-if use AlphaCutoff
    }
}
