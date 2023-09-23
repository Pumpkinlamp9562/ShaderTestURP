// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "SCS/VFX/TransparentArm"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin]_Offset("Offset", Range( 0 , 1)) = -0.63
		_FadeOffset("FadeOffset", Float) = 0.46
		_FadeColOffset("FadeColOffset", Float) = 0.46
		_OffsetMinMax("OffsetMinMax", Vector) = (0.08,0.11,0,0)
		_Fresnal("Fresnal", Vector) = (0.02,0.5,3.25,0)
		[HDR]_Color0("Color 0", Color) = (0,0,0,0)
		[HDR]_Color1("Color 1", Color) = (0,0,0,0)
		[HDR]_Color2("Color 2", Color) = (0,0,0,0)
		_FlickStrenght("Flick Strenght", Range( 0 , 2)) = 0.4
		_ScollTimeScale("Scoll Time Scale", Range( 0 , 0.076)) = 0.01081722
		_ScrollScale("Scroll Scale", Float) = -4.4
		_Linesnumber("Lines number", Float) = 1500
		[HideInInspector]_Grid("Grid", 2D) = "white" {}
		_Inverst("Inverst", Float) = -0.85
		_Alpha("Alpha", Float) = 1.85
		_FadeStrenght("FadeStrenght", Float) = 0
		_TextureSample0("Texture Sample 0", 2D) = "white" {}
		_SkinGrowOffset("SkinGrowOffset", Float) = 0.9
		[ASEEnd]_Float3("Float 3", Range( -0.5 , 0.5)) = 0
		[HideInInspector] _texcoord( "", 2D ) = "white" {}

		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25
	}

	SubShader
	{
		LOD 0

		
		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" }
		
		Cull Back
		AlphaToMask Off
		
		HLSLINCLUDE
		#pragma target 2.0

		#pragma prefer_hlslcc gles
		#pragma exclude_renderers d3d11_9x 

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}
		
		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS

		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			
			#pragma multi_compile_instancing
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"

			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				#ifdef ASE_FOG
				float fogFactor : TEXCOORD2;
				#endif
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color0;
			float4 _TextureSample0_ST;
			float4 _Grid_ST;
			float4 _Color1;
			float4 _Color2;
			float3 _Fresnal;
			float2 _OffsetMinMax;
			float _SkinGrowOffset;
			float _Float3;
			float _FlickStrenght;
			float _FadeOffset;
			float _FadeColOffset;
			float _ScollTimeScale;
			float _Linesnumber;
			float _ScrollScale;
			float _Inverst;
			float _Offset;
			float _FadeStrenght;
			float _Alpha;
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _Grid;
			sampler2D _TextureSample0;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			inline float Dither8x8Bayer( int x, int y )
			{
				const float dither[ 64 ] = {
			 1, 49, 13, 61,  4, 52, 16, 64,
			33, 17, 45, 29, 36, 20, 48, 32,
			 9, 57,  5, 53, 12, 60,  8, 56,
			41, 25, 37, 21, 44, 28, 40, 24,
			 3, 51, 15, 63,  2, 50, 14, 62,
			35, 19, 47, 31, 34, 18, 46, 30,
			11, 59,  7, 55, 10, 58,  6, 54,
			43, 27, 39, 23, 42, 26, 38, 22};
				int r = y * 8 + x;
				return dither[r] / 64; // same # of instructions as pre-dividing due to compiler magic
			}
			
			
			VertexOutput VertexFunction ( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float LocalPos295 = v.vertex.xyz.x;
				float temp_output_450_0 = (-0.4 + (_Offset - 0.0) * (1.3 - -0.4) / (1.0 - 0.0));
				float smoothstepResult15 = smoothstep( _OffsetMinMax.x , _OffsetMinMax.y , saturate( ( LocalPos295 + temp_output_450_0 ) ));
				float Offset336 = smoothstepResult15;
				float temp_output_35_0_g19 = Offset336;
				float2 break41_g19 = float2( 0.7,0 );
				float temp_output_36_0_g19 = LocalPos295;
				float temp_output_38_0_g19 = temp_output_450_0;
				float smoothstepResult23_g19 = smoothstep( break41_g19.x , break41_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + _FadeColOffset + temp_output_38_0_g19 ) ) ));
				float temp_output_27_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult23_g19 ) );
				float smoothstepResult469 = smoothstep( 0.33 , 1.0 , temp_output_27_0_g19);
				float2 break43_g19 = float2( 0.08,0.47 );
				float smoothstepResult18_g19 = smoothstep( break43_g19.x , break43_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + temp_output_38_0_g19 + _FadeOffset ) ) ));
				float temp_output_7_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult18_g19 ) );
				float2 break47_g19 = float2( 0,50 );
				float LocalPosY302 = v.vertex.xyz.y;
				float2 appendResult20_g19 = (float2(( break47_g19.x * temp_output_36_0_g19 ) , ( break47_g19.y * LocalPosY302 )));
				float simplePerlin2D24_g19 = snoise( appendResult20_g19*7.08 );
				simplePerlin2D24_g19 = simplePerlin2D24_g19*0.5 + 0.5;
				float NoiseClip367 = step( -0.03 , ( smoothstepResult469 - ( temp_output_7_0_g19 * simplePerlin2D24_g19 ) ) );
				float temp_output_35_0_g18 = Offset336;
				float2 break41_g18 = float2( 0.7,0 );
				float temp_output_36_0_g18 = LocalPos295;
				float temp_output_38_0_g18 = ( temp_output_450_0 - _SkinGrowOffset );
				float smoothstepResult23_g18 = smoothstep( break41_g18.x , break41_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + _FadeColOffset + temp_output_38_0_g18 ) ) ));
				float temp_output_27_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult23_g18 ) );
				float smoothstepResult472 = smoothstep( -0.7 , 1.0 , temp_output_27_0_g18);
				float2 break43_g18 = float2( 0.08,0.47 );
				float smoothstepResult18_g18 = smoothstep( break43_g18.x , break43_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + temp_output_38_0_g18 + _FadeOffset ) ) ));
				float temp_output_7_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult18_g18 ) );
				float2 break47_g18 = float2( 0,50 );
				float2 appendResult20_g18 = (float2(( break47_g18.x * temp_output_36_0_g18 ) , ( break47_g18.y * LocalPosY302 )));
				float simplePerlin2D24_g18 = snoise( appendResult20_g18*7.08 );
				simplePerlin2D24_g18 = simplePerlin2D24_g18*0.5 + 0.5;
				float temp_output_474_0 = step( _Float3 , ( smoothstepResult472 - ( temp_output_7_0_g18 * simplePerlin2D24_g18 ) ) );
				float NoiseClip2414 = temp_output_474_0;
				float mulTime175 = _TimeParameters.x * 0.2;
				float2 temp_cast_0 = (mulTime175).xx;
				float simplePerlin2D172 = snoise( temp_cast_0*8.0 );
				simplePerlin2D172 = simplePerlin2D172*0.5 + 0.5;
				float mulTime233 = _TimeParameters.x * 0.2;
				float2 temp_cast_1 = (( LocalPos295 - mulTime233 )).xx;
				float simplePerlin2D234 = snoise( temp_cast_1*20.0 );
				simplePerlin2D234 = simplePerlin2D234*0.5 + 0.5;
				float smoothstepResult263 = smoothstep( 0.58 , 1.0 , simplePerlin2D234);
				float4 appendResult249 = (float4(( ( 1.0 - ( saturate( NoiseClip367 ) + saturate( NoiseClip2414 ) ) ) * _FadeStrenght ) , saturate( ( pow( simplePerlin2D172 , 2.17 ) * smoothstepResult263 * 0.0045 ) ) , 0.0 , 0.0));
				float NoiseColor2415 = step( 0.03 , ( min( temp_output_7_0_g18 , temp_output_27_0_g18 ) * simplePerlin2D24_g18 ) );
				float temp_output_445_0 = ( NoiseClip2414 + NoiseColor2415 );
				float4 lerpResult429 = lerp( appendResult249 , float4( 0,0,0,0 ) , temp_output_445_0);
				
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord4.xyz = ase_worldNormal;
				
				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord6 = screenPos;
				
				o.ase_texcoord3.xy = v.ase_texcoord.xy;
				o.ase_texcoord5 = v.vertex;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord3.zw = 0;
				o.ase_texcoord4.w = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = lerpResult429.xyz;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				#ifdef ASE_FOG
				o.fogFactor = ComputeFogFactor( positionCS.z );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag ( VertexOutput IN  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif
				float2 uv_Grid = IN.ase_texcoord3.xy * _Grid_ST.xy + _Grid_ST.zw;
				float grayscale277 = Luminance(tex2D( _Grid, uv_Grid ).rgb);
				float3 ase_worldViewDir = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				ase_worldViewDir = normalize(ase_worldViewDir);
				float3 ase_worldNormal = IN.ase_texcoord4.xyz;
				float fresnelNdotV43 = dot( ase_worldNormal, ase_worldViewDir );
				float fresnelNode43 = ( _Fresnal.x + _Fresnal.y * pow( 1.0 - fresnelNdotV43, _Fresnal.z ) );
				float Fresnel329 = fresnelNode43;
				float LocalPos295 = IN.ase_texcoord5.xyz.x;
				float mulTime188 = _TimeParameters.x * _ScollTimeScale;
				float2 _Scrollfade = float2(0.83,0.39);
				float temp_output_194_0 = frac( ( ( LocalPos295 - mulTime188 ) * _ScrollScale ) );
				float smoothstepResult196 = smoothstep( _Scrollfade.x , _Scrollfade.y , temp_output_194_0);
				float2 _ScrollEdgefade = float2(0.04,0.8);
				float smoothstepResult197 = smoothstep( _ScrollEdgefade.x , _ScrollEdgefade.y , temp_output_194_0);
				float4 lerpResult201 = lerp( _Color1 , _Color0 , ( ( 0.25 * grayscale277 ) + ( Fresnel329 * ( ( saturate( sin( ( ( LocalPos295 - mulTime188 ) * _Linesnumber ) ) ) * ( 1.0 - smoothstepResult196 ) ) + ( 1.0 - smoothstepResult197 ) ) * _Inverst ) ));
				float4 lerpResult284 = lerp( _Color2 , lerpResult201 , ( Fresnel329 * 28.5 ));
				float temp_output_450_0 = (-0.4 + (_Offset - 0.0) * (1.3 - -0.4) / (1.0 - 0.0));
				float smoothstepResult15 = smoothstep( _OffsetMinMax.x , _OffsetMinMax.y , saturate( ( LocalPos295 + temp_output_450_0 ) ));
				float Offset336 = smoothstepResult15;
				float temp_output_35_0_g19 = Offset336;
				float2 break43_g19 = float2( 0.08,0.47 );
				float temp_output_36_0_g19 = LocalPos295;
				float temp_output_38_0_g19 = temp_output_450_0;
				float smoothstepResult18_g19 = smoothstep( break43_g19.x , break43_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + temp_output_38_0_g19 + _FadeOffset ) ) ));
				float temp_output_7_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult18_g19 ) );
				float2 break41_g19 = float2( 0.7,0 );
				float smoothstepResult23_g19 = smoothstep( break41_g19.x , break41_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + _FadeColOffset + temp_output_38_0_g19 ) ) ));
				float temp_output_27_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult23_g19 ) );
				float2 break47_g19 = float2( 0,50 );
				float LocalPosY302 = IN.ase_texcoord5.xyz.y;
				float2 appendResult20_g19 = (float2(( break47_g19.x * temp_output_36_0_g19 ) , ( break47_g19.y * LocalPosY302 )));
				float simplePerlin2D24_g19 = snoise( appendResult20_g19*7.08 );
				simplePerlin2D24_g19 = simplePerlin2D24_g19*0.5 + 0.5;
				float NoiseColor327 = step( 0.03 , ( min( temp_output_7_0_g19 , temp_output_27_0_g19 ) * simplePerlin2D24_g19 ) );
				float2 uv_TextureSample0 = IN.ase_texcoord3.xy * _TextureSample0_ST.xy + _TextureSample0_ST.zw;
				float temp_output_35_0_g18 = Offset336;
				float2 break41_g18 = float2( 0.7,0 );
				float temp_output_36_0_g18 = LocalPos295;
				float temp_output_38_0_g18 = ( temp_output_450_0 - _SkinGrowOffset );
				float smoothstepResult23_g18 = smoothstep( break41_g18.x , break41_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + _FadeColOffset + temp_output_38_0_g18 ) ) ));
				float temp_output_27_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult23_g18 ) );
				float smoothstepResult472 = smoothstep( -0.7 , 1.0 , temp_output_27_0_g18);
				float2 break43_g18 = float2( 0.08,0.47 );
				float smoothstepResult18_g18 = smoothstep( break43_g18.x , break43_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + temp_output_38_0_g18 + _FadeOffset ) ) ));
				float temp_output_7_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult18_g18 ) );
				float2 break47_g18 = float2( 0,50 );
				float2 appendResult20_g18 = (float2(( break47_g18.x * temp_output_36_0_g18 ) , ( break47_g18.y * LocalPosY302 )));
				float simplePerlin2D24_g18 = snoise( appendResult20_g18*7.08 );
				simplePerlin2D24_g18 = simplePerlin2D24_g18*0.5 + 0.5;
				float temp_output_474_0 = step( _Float3 , ( smoothstepResult472 - ( temp_output_7_0_g18 * simplePerlin2D24_g18 ) ) );
				float NoiseClip2414 = temp_output_474_0;
				float4 lerpResult419 = lerp( ( lerpResult284 + ( NoiseColor327 * 0.5 ) ) , tex2D( _TextureSample0, uv_TextureSample0 ) , NoiseClip2414);
				float NoiseColor2415 = step( 0.03 , ( min( temp_output_7_0_g18 , temp_output_27_0_g18 ) * simplePerlin2D24_g18 ) );
				float4 color446 = IsGammaSpace() ? float4(1,1,1,0) : float4(1,1,1,0);
				float4 lerpResult448 = lerp( ( lerpResult419 + NoiseColor2415 ) , color446 , 0.0);
				
				float4 screenPos = IN.ase_texcoord6;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen464 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither464 = Dither8x8Bayer( fmod(clipScreen464.x, 8), fmod(clipScreen464.y, 8) );
				float mulTime175 = _TimeParameters.x * 0.2;
				float2 temp_cast_2 = (mulTime175).xx;
				float simplePerlin2D172 = snoise( temp_cast_2*8.0 );
				simplePerlin2D172 = simplePerlin2D172*0.5 + 0.5;
				float smoothstepResult469 = smoothstep( 0.33 , 1.0 , temp_output_27_0_g19);
				float NoiseClip367 = step( -0.03 , ( smoothstepResult469 - ( temp_output_7_0_g19 * simplePerlin2D24_g19 ) ) );
				float temp_output_445_0 = ( NoiseClip2414 + NoiseColor2415 );
				float lerpResult427 = lerp( ( NoiseColor327 + ( ( Fresnel329 * Offset336 ) * pow( simplePerlin2D172 , _FlickStrenght ) * _Alpha * NoiseClip367 ) ) , 1.0 , temp_output_445_0);
				dither464 = step( dither464, lerpResult427 );
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = lerpResult448.rgb;
				float Alpha = dither464;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					clip( Alpha - AlphaClipThreshold );
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#ifdef ASE_FOG
					Color = MixFog( Color, IN.fogFactor );
				#endif

				return half4( Color, Alpha );
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual
			AlphaToMask Off
			ColorMask 0

			HLSLPROGRAM
			
			#pragma multi_compile_instancing
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag
#if ASE_SRP_VERSION >= 110000
			#pragma multi_compile _ _CASTING_PUNCTUAL_LIGHT_SHADOW
#endif
			#define SHADERPASS SHADERPASS_SHADOWCASTER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_NORMAL


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color0;
			float4 _TextureSample0_ST;
			float4 _Grid_ST;
			float4 _Color1;
			float4 _Color2;
			float3 _Fresnal;
			float2 _OffsetMinMax;
			float _SkinGrowOffset;
			float _Float3;
			float _FlickStrenght;
			float _FadeOffset;
			float _FadeColOffset;
			float _ScollTimeScale;
			float _Linesnumber;
			float _ScrollScale;
			float _Inverst;
			float _Offset;
			float _FadeStrenght;
			float _Alpha;
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			

			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			inline float Dither8x8Bayer( int x, int y )
			{
				const float dither[ 64 ] = {
			 1, 49, 13, 61,  4, 52, 16, 64,
			33, 17, 45, 29, 36, 20, 48, 32,
			 9, 57,  5, 53, 12, 60,  8, 56,
			41, 25, 37, 21, 44, 28, 40, 24,
			 3, 51, 15, 63,  2, 50, 14, 62,
			35, 19, 47, 31, 34, 18, 46, 30,
			11, 59,  7, 55, 10, 58,  6, 54,
			43, 27, 39, 23, 42, 26, 38, 22};
				int r = y * 8 + x;
				return dither[r] / 64; // same # of instructions as pre-dividing due to compiler magic
			}
			

			float3 _LightDirection;
#if ASE_SRP_VERSION >= 110000 
			float3 _LightPosition;
#endif
			VertexOutput VertexFunction( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float LocalPos295 = v.vertex.xyz.x;
				float temp_output_450_0 = (-0.4 + (_Offset - 0.0) * (1.3 - -0.4) / (1.0 - 0.0));
				float smoothstepResult15 = smoothstep( _OffsetMinMax.x , _OffsetMinMax.y , saturate( ( LocalPos295 + temp_output_450_0 ) ));
				float Offset336 = smoothstepResult15;
				float temp_output_35_0_g19 = Offset336;
				float2 break41_g19 = float2( 0.7,0 );
				float temp_output_36_0_g19 = LocalPos295;
				float temp_output_38_0_g19 = temp_output_450_0;
				float smoothstepResult23_g19 = smoothstep( break41_g19.x , break41_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + _FadeColOffset + temp_output_38_0_g19 ) ) ));
				float temp_output_27_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult23_g19 ) );
				float smoothstepResult469 = smoothstep( 0.33 , 1.0 , temp_output_27_0_g19);
				float2 break43_g19 = float2( 0.08,0.47 );
				float smoothstepResult18_g19 = smoothstep( break43_g19.x , break43_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + temp_output_38_0_g19 + _FadeOffset ) ) ));
				float temp_output_7_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult18_g19 ) );
				float2 break47_g19 = float2( 0,50 );
				float LocalPosY302 = v.vertex.xyz.y;
				float2 appendResult20_g19 = (float2(( break47_g19.x * temp_output_36_0_g19 ) , ( break47_g19.y * LocalPosY302 )));
				float simplePerlin2D24_g19 = snoise( appendResult20_g19*7.08 );
				simplePerlin2D24_g19 = simplePerlin2D24_g19*0.5 + 0.5;
				float NoiseClip367 = step( -0.03 , ( smoothstepResult469 - ( temp_output_7_0_g19 * simplePerlin2D24_g19 ) ) );
				float temp_output_35_0_g18 = Offset336;
				float2 break41_g18 = float2( 0.7,0 );
				float temp_output_36_0_g18 = LocalPos295;
				float temp_output_38_0_g18 = ( temp_output_450_0 - _SkinGrowOffset );
				float smoothstepResult23_g18 = smoothstep( break41_g18.x , break41_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + _FadeColOffset + temp_output_38_0_g18 ) ) ));
				float temp_output_27_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult23_g18 ) );
				float smoothstepResult472 = smoothstep( -0.7 , 1.0 , temp_output_27_0_g18);
				float2 break43_g18 = float2( 0.08,0.47 );
				float smoothstepResult18_g18 = smoothstep( break43_g18.x , break43_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + temp_output_38_0_g18 + _FadeOffset ) ) ));
				float temp_output_7_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult18_g18 ) );
				float2 break47_g18 = float2( 0,50 );
				float2 appendResult20_g18 = (float2(( break47_g18.x * temp_output_36_0_g18 ) , ( break47_g18.y * LocalPosY302 )));
				float simplePerlin2D24_g18 = snoise( appendResult20_g18*7.08 );
				simplePerlin2D24_g18 = simplePerlin2D24_g18*0.5 + 0.5;
				float temp_output_474_0 = step( _Float3 , ( smoothstepResult472 - ( temp_output_7_0_g18 * simplePerlin2D24_g18 ) ) );
				float NoiseClip2414 = temp_output_474_0;
				float mulTime175 = _TimeParameters.x * 0.2;
				float2 temp_cast_0 = (mulTime175).xx;
				float simplePerlin2D172 = snoise( temp_cast_0*8.0 );
				simplePerlin2D172 = simplePerlin2D172*0.5 + 0.5;
				float mulTime233 = _TimeParameters.x * 0.2;
				float2 temp_cast_1 = (( LocalPos295 - mulTime233 )).xx;
				float simplePerlin2D234 = snoise( temp_cast_1*20.0 );
				simplePerlin2D234 = simplePerlin2D234*0.5 + 0.5;
				float smoothstepResult263 = smoothstep( 0.58 , 1.0 , simplePerlin2D234);
				float4 appendResult249 = (float4(( ( 1.0 - ( saturate( NoiseClip367 ) + saturate( NoiseClip2414 ) ) ) * _FadeStrenght ) , saturate( ( pow( simplePerlin2D172 , 2.17 ) * smoothstepResult263 * 0.0045 ) ) , 0.0 , 0.0));
				float NoiseColor2415 = step( 0.03 , ( min( temp_output_7_0_g18 , temp_output_27_0_g18 ) * simplePerlin2D24_g18 ) );
				float temp_output_445_0 = ( NoiseClip2414 + NoiseColor2415 );
				float4 lerpResult429 = lerp( appendResult249 , float4( 0,0,0,0 ) , temp_output_445_0);
				
				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord2 = screenPos;
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord4.xyz = ase_worldNormal;
				
				o.ase_texcoord3 = v.vertex;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord4.w = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = lerpResult429.xyz;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				float3 normalWS = TransformObjectToWorldDir( v.ase_normal );
#if ASE_SRP_VERSION >= 110000 
			#if _CASTING_PUNCTUAL_LIGHT_SHADOW
				float3 lightDirectionWS = normalize(_LightPosition - positionWS);
			#else
				float3 lightDirectionWS = _LightDirection;
			#endif
				float4 clipPos = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));
			#if UNITY_REVERSED_Z
				clipPos.z = min(clipPos.z, UNITY_NEAR_CLIP_VALUE);
			#else
				clipPos.z = max(clipPos.z, UNITY_NEAR_CLIP_VALUE);
			#endif
#else
				float4 clipPos = TransformWorldToHClip( ApplyShadowBias( positionWS, normalWS, _LightDirection ) );
				#if UNITY_REVERSED_Z
					clipPos.z = min(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
				#else
					clipPos.z = max(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
				#endif
#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = clipPos;

				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float4 screenPos = IN.ase_texcoord2;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen464 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither464 = Dither8x8Bayer( fmod(clipScreen464.x, 8), fmod(clipScreen464.y, 8) );
				float LocalPos295 = IN.ase_texcoord3.xyz.x;
				float temp_output_450_0 = (-0.4 + (_Offset - 0.0) * (1.3 - -0.4) / (1.0 - 0.0));
				float smoothstepResult15 = smoothstep( _OffsetMinMax.x , _OffsetMinMax.y , saturate( ( LocalPos295 + temp_output_450_0 ) ));
				float Offset336 = smoothstepResult15;
				float temp_output_35_0_g19 = Offset336;
				float2 break43_g19 = float2( 0.08,0.47 );
				float temp_output_36_0_g19 = LocalPos295;
				float temp_output_38_0_g19 = temp_output_450_0;
				float smoothstepResult18_g19 = smoothstep( break43_g19.x , break43_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + temp_output_38_0_g19 + _FadeOffset ) ) ));
				float temp_output_7_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult18_g19 ) );
				float2 break41_g19 = float2( 0.7,0 );
				float smoothstepResult23_g19 = smoothstep( break41_g19.x , break41_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + _FadeColOffset + temp_output_38_0_g19 ) ) ));
				float temp_output_27_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult23_g19 ) );
				float2 break47_g19 = float2( 0,50 );
				float LocalPosY302 = IN.ase_texcoord3.xyz.y;
				float2 appendResult20_g19 = (float2(( break47_g19.x * temp_output_36_0_g19 ) , ( break47_g19.y * LocalPosY302 )));
				float simplePerlin2D24_g19 = snoise( appendResult20_g19*7.08 );
				simplePerlin2D24_g19 = simplePerlin2D24_g19*0.5 + 0.5;
				float NoiseColor327 = step( 0.03 , ( min( temp_output_7_0_g19 , temp_output_27_0_g19 ) * simplePerlin2D24_g19 ) );
				float3 ase_worldViewDir = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				ase_worldViewDir = normalize(ase_worldViewDir);
				float3 ase_worldNormal = IN.ase_texcoord4.xyz;
				float fresnelNdotV43 = dot( ase_worldNormal, ase_worldViewDir );
				float fresnelNode43 = ( _Fresnal.x + _Fresnal.y * pow( 1.0 - fresnelNdotV43, _Fresnal.z ) );
				float Fresnel329 = fresnelNode43;
				float mulTime175 = _TimeParameters.x * 0.2;
				float2 temp_cast_0 = (mulTime175).xx;
				float simplePerlin2D172 = snoise( temp_cast_0*8.0 );
				simplePerlin2D172 = simplePerlin2D172*0.5 + 0.5;
				float smoothstepResult469 = smoothstep( 0.33 , 1.0 , temp_output_27_0_g19);
				float NoiseClip367 = step( -0.03 , ( smoothstepResult469 - ( temp_output_7_0_g19 * simplePerlin2D24_g19 ) ) );
				float temp_output_35_0_g18 = Offset336;
				float2 break41_g18 = float2( 0.7,0 );
				float temp_output_36_0_g18 = LocalPos295;
				float temp_output_38_0_g18 = ( temp_output_450_0 - _SkinGrowOffset );
				float smoothstepResult23_g18 = smoothstep( break41_g18.x , break41_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + _FadeColOffset + temp_output_38_0_g18 ) ) ));
				float temp_output_27_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult23_g18 ) );
				float smoothstepResult472 = smoothstep( -0.7 , 1.0 , temp_output_27_0_g18);
				float2 break43_g18 = float2( 0.08,0.47 );
				float smoothstepResult18_g18 = smoothstep( break43_g18.x , break43_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + temp_output_38_0_g18 + _FadeOffset ) ) ));
				float temp_output_7_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult18_g18 ) );
				float2 break47_g18 = float2( 0,50 );
				float2 appendResult20_g18 = (float2(( break47_g18.x * temp_output_36_0_g18 ) , ( break47_g18.y * LocalPosY302 )));
				float simplePerlin2D24_g18 = snoise( appendResult20_g18*7.08 );
				simplePerlin2D24_g18 = simplePerlin2D24_g18*0.5 + 0.5;
				float temp_output_474_0 = step( _Float3 , ( smoothstepResult472 - ( temp_output_7_0_g18 * simplePerlin2D24_g18 ) ) );
				float NoiseClip2414 = temp_output_474_0;
				float NoiseColor2415 = step( 0.03 , ( min( temp_output_7_0_g18 , temp_output_27_0_g18 ) * simplePerlin2D24_g18 ) );
				float temp_output_445_0 = ( NoiseClip2414 + NoiseColor2415 );
				float lerpResult427 = lerp( ( NoiseColor327 + ( ( Fresnel329 * Offset336 ) * pow( simplePerlin2D172 , _FlickStrenght ) * _Alpha * NoiseClip367 ) ) , 1.0 , temp_output_445_0);
				dither464 = step( dither464, lerpResult427 );
				
				float Alpha = dither464;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					#ifdef _ALPHATEST_SHADOW_ON
						clip(Alpha - AlphaClipThresholdShadow);
					#else
						clip(Alpha - AlphaClipThreshold);
					#endif
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				return 0;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM
			
			#pragma multi_compile_instancing
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_NORMAL


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color0;
			float4 _TextureSample0_ST;
			float4 _Grid_ST;
			float4 _Color1;
			float4 _Color2;
			float3 _Fresnal;
			float2 _OffsetMinMax;
			float _SkinGrowOffset;
			float _Float3;
			float _FlickStrenght;
			float _FadeOffset;
			float _FadeColOffset;
			float _ScollTimeScale;
			float _Linesnumber;
			float _ScrollScale;
			float _Inverst;
			float _Offset;
			float _FadeStrenght;
			float _Alpha;
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			

			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			inline float Dither8x8Bayer( int x, int y )
			{
				const float dither[ 64 ] = {
			 1, 49, 13, 61,  4, 52, 16, 64,
			33, 17, 45, 29, 36, 20, 48, 32,
			 9, 57,  5, 53, 12, 60,  8, 56,
			41, 25, 37, 21, 44, 28, 40, 24,
			 3, 51, 15, 63,  2, 50, 14, 62,
			35, 19, 47, 31, 34, 18, 46, 30,
			11, 59,  7, 55, 10, 58,  6, 54,
			43, 27, 39, 23, 42, 26, 38, 22};
				int r = y * 8 + x;
				return dither[r] / 64; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float LocalPos295 = v.vertex.xyz.x;
				float temp_output_450_0 = (-0.4 + (_Offset - 0.0) * (1.3 - -0.4) / (1.0 - 0.0));
				float smoothstepResult15 = smoothstep( _OffsetMinMax.x , _OffsetMinMax.y , saturate( ( LocalPos295 + temp_output_450_0 ) ));
				float Offset336 = smoothstepResult15;
				float temp_output_35_0_g19 = Offset336;
				float2 break41_g19 = float2( 0.7,0 );
				float temp_output_36_0_g19 = LocalPos295;
				float temp_output_38_0_g19 = temp_output_450_0;
				float smoothstepResult23_g19 = smoothstep( break41_g19.x , break41_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + _FadeColOffset + temp_output_38_0_g19 ) ) ));
				float temp_output_27_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult23_g19 ) );
				float smoothstepResult469 = smoothstep( 0.33 , 1.0 , temp_output_27_0_g19);
				float2 break43_g19 = float2( 0.08,0.47 );
				float smoothstepResult18_g19 = smoothstep( break43_g19.x , break43_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + temp_output_38_0_g19 + _FadeOffset ) ) ));
				float temp_output_7_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult18_g19 ) );
				float2 break47_g19 = float2( 0,50 );
				float LocalPosY302 = v.vertex.xyz.y;
				float2 appendResult20_g19 = (float2(( break47_g19.x * temp_output_36_0_g19 ) , ( break47_g19.y * LocalPosY302 )));
				float simplePerlin2D24_g19 = snoise( appendResult20_g19*7.08 );
				simplePerlin2D24_g19 = simplePerlin2D24_g19*0.5 + 0.5;
				float NoiseClip367 = step( -0.03 , ( smoothstepResult469 - ( temp_output_7_0_g19 * simplePerlin2D24_g19 ) ) );
				float temp_output_35_0_g18 = Offset336;
				float2 break41_g18 = float2( 0.7,0 );
				float temp_output_36_0_g18 = LocalPos295;
				float temp_output_38_0_g18 = ( temp_output_450_0 - _SkinGrowOffset );
				float smoothstepResult23_g18 = smoothstep( break41_g18.x , break41_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + _FadeColOffset + temp_output_38_0_g18 ) ) ));
				float temp_output_27_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult23_g18 ) );
				float smoothstepResult472 = smoothstep( -0.7 , 1.0 , temp_output_27_0_g18);
				float2 break43_g18 = float2( 0.08,0.47 );
				float smoothstepResult18_g18 = smoothstep( break43_g18.x , break43_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + temp_output_38_0_g18 + _FadeOffset ) ) ));
				float temp_output_7_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult18_g18 ) );
				float2 break47_g18 = float2( 0,50 );
				float2 appendResult20_g18 = (float2(( break47_g18.x * temp_output_36_0_g18 ) , ( break47_g18.y * LocalPosY302 )));
				float simplePerlin2D24_g18 = snoise( appendResult20_g18*7.08 );
				simplePerlin2D24_g18 = simplePerlin2D24_g18*0.5 + 0.5;
				float temp_output_474_0 = step( _Float3 , ( smoothstepResult472 - ( temp_output_7_0_g18 * simplePerlin2D24_g18 ) ) );
				float NoiseClip2414 = temp_output_474_0;
				float mulTime175 = _TimeParameters.x * 0.2;
				float2 temp_cast_0 = (mulTime175).xx;
				float simplePerlin2D172 = snoise( temp_cast_0*8.0 );
				simplePerlin2D172 = simplePerlin2D172*0.5 + 0.5;
				float mulTime233 = _TimeParameters.x * 0.2;
				float2 temp_cast_1 = (( LocalPos295 - mulTime233 )).xx;
				float simplePerlin2D234 = snoise( temp_cast_1*20.0 );
				simplePerlin2D234 = simplePerlin2D234*0.5 + 0.5;
				float smoothstepResult263 = smoothstep( 0.58 , 1.0 , simplePerlin2D234);
				float4 appendResult249 = (float4(( ( 1.0 - ( saturate( NoiseClip367 ) + saturate( NoiseClip2414 ) ) ) * _FadeStrenght ) , saturate( ( pow( simplePerlin2D172 , 2.17 ) * smoothstepResult263 * 0.0045 ) ) , 0.0 , 0.0));
				float NoiseColor2415 = step( 0.03 , ( min( temp_output_7_0_g18 , temp_output_27_0_g18 ) * simplePerlin2D24_g18 ) );
				float temp_output_445_0 = ( NoiseClip2414 + NoiseColor2415 );
				float4 lerpResult429 = lerp( appendResult249 , float4( 0,0,0,0 ) , temp_output_445_0);
				
				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord2 = screenPos;
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord4.xyz = ase_worldNormal;
				
				o.ase_texcoord3 = v.vertex;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord4.w = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = lerpResult429.xyz;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = TransformWorldToHClip( positionWS );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float4 screenPos = IN.ase_texcoord2;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen464 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither464 = Dither8x8Bayer( fmod(clipScreen464.x, 8), fmod(clipScreen464.y, 8) );
				float LocalPos295 = IN.ase_texcoord3.xyz.x;
				float temp_output_450_0 = (-0.4 + (_Offset - 0.0) * (1.3 - -0.4) / (1.0 - 0.0));
				float smoothstepResult15 = smoothstep( _OffsetMinMax.x , _OffsetMinMax.y , saturate( ( LocalPos295 + temp_output_450_0 ) ));
				float Offset336 = smoothstepResult15;
				float temp_output_35_0_g19 = Offset336;
				float2 break43_g19 = float2( 0.08,0.47 );
				float temp_output_36_0_g19 = LocalPos295;
				float temp_output_38_0_g19 = temp_output_450_0;
				float smoothstepResult18_g19 = smoothstep( break43_g19.x , break43_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + temp_output_38_0_g19 + _FadeOffset ) ) ));
				float temp_output_7_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult18_g19 ) );
				float2 break41_g19 = float2( 0.7,0 );
				float smoothstepResult23_g19 = smoothstep( break41_g19.x , break41_g19.y , ( 1.0 - saturate( ( temp_output_36_0_g19 + _FadeColOffset + temp_output_38_0_g19 ) ) ));
				float temp_output_27_0_g19 = ( temp_output_35_0_g19 * saturate( smoothstepResult23_g19 ) );
				float2 break47_g19 = float2( 0,50 );
				float LocalPosY302 = IN.ase_texcoord3.xyz.y;
				float2 appendResult20_g19 = (float2(( break47_g19.x * temp_output_36_0_g19 ) , ( break47_g19.y * LocalPosY302 )));
				float simplePerlin2D24_g19 = snoise( appendResult20_g19*7.08 );
				simplePerlin2D24_g19 = simplePerlin2D24_g19*0.5 + 0.5;
				float NoiseColor327 = step( 0.03 , ( min( temp_output_7_0_g19 , temp_output_27_0_g19 ) * simplePerlin2D24_g19 ) );
				float3 ase_worldViewDir = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				ase_worldViewDir = normalize(ase_worldViewDir);
				float3 ase_worldNormal = IN.ase_texcoord4.xyz;
				float fresnelNdotV43 = dot( ase_worldNormal, ase_worldViewDir );
				float fresnelNode43 = ( _Fresnal.x + _Fresnal.y * pow( 1.0 - fresnelNdotV43, _Fresnal.z ) );
				float Fresnel329 = fresnelNode43;
				float mulTime175 = _TimeParameters.x * 0.2;
				float2 temp_cast_0 = (mulTime175).xx;
				float simplePerlin2D172 = snoise( temp_cast_0*8.0 );
				simplePerlin2D172 = simplePerlin2D172*0.5 + 0.5;
				float smoothstepResult469 = smoothstep( 0.33 , 1.0 , temp_output_27_0_g19);
				float NoiseClip367 = step( -0.03 , ( smoothstepResult469 - ( temp_output_7_0_g19 * simplePerlin2D24_g19 ) ) );
				float temp_output_35_0_g18 = Offset336;
				float2 break41_g18 = float2( 0.7,0 );
				float temp_output_36_0_g18 = LocalPos295;
				float temp_output_38_0_g18 = ( temp_output_450_0 - _SkinGrowOffset );
				float smoothstepResult23_g18 = smoothstep( break41_g18.x , break41_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + _FadeColOffset + temp_output_38_0_g18 ) ) ));
				float temp_output_27_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult23_g18 ) );
				float smoothstepResult472 = smoothstep( -0.7 , 1.0 , temp_output_27_0_g18);
				float2 break43_g18 = float2( 0.08,0.47 );
				float smoothstepResult18_g18 = smoothstep( break43_g18.x , break43_g18.y , ( 1.0 - saturate( ( temp_output_36_0_g18 + temp_output_38_0_g18 + _FadeOffset ) ) ));
				float temp_output_7_0_g18 = ( temp_output_35_0_g18 * saturate( smoothstepResult18_g18 ) );
				float2 break47_g18 = float2( 0,50 );
				float2 appendResult20_g18 = (float2(( break47_g18.x * temp_output_36_0_g18 ) , ( break47_g18.y * LocalPosY302 )));
				float simplePerlin2D24_g18 = snoise( appendResult20_g18*7.08 );
				simplePerlin2D24_g18 = simplePerlin2D24_g18*0.5 + 0.5;
				float temp_output_474_0 = step( _Float3 , ( smoothstepResult472 - ( temp_output_7_0_g18 * simplePerlin2D24_g18 ) ) );
				float NoiseClip2414 = temp_output_474_0;
				float NoiseColor2415 = step( 0.03 , ( min( temp_output_7_0_g18 , temp_output_27_0_g18 ) * simplePerlin2D24_g18 ) );
				float temp_output_445_0 = ( NoiseClip2414 + NoiseColor2415 );
				float lerpResult427 = lerp( ( NoiseColor327 + ( ( Fresnel329 * Offset336 ) * pow( simplePerlin2D172 , _FlickStrenght ) * _Alpha * NoiseClip367 ) ) , 1.0 , temp_output_445_0);
				dither464 = step( dither464, lerpResult427 );
				
				float Alpha = dither464;
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				return 0;
			}
			ENDHLSL
		}

	
	}
	
	CustomEditor "UnityEditor.ShaderGraph.PBRMasterGUI"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=18921
1910;-6;1920;1678;2870.025;64.81606;1.437837;True;False
Node;AmplifyShaderEditor.PosVertexDataNode;296;-1409.827,181.6865;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;295;-1194.547,171.907;Inherit;False;LocalPos;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;410;-2296.554,793.2197;Inherit;False;Property;_Offset;Offset;0;0;Create;True;0;0;0;False;0;False;-0.63;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;297;-2008.989,-56.644;Inherit;False;295;LocalPos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;450;-2002.942,719.2255;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;-0.4;False;4;FLOAT;1.3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;12;-1800.425,-51.2283;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;16;-1677.771,22.69634;Inherit;False;Property;_OffsetMinMax;OffsetMinMax;4;0;Create;True;0;0;0;False;0;False;0.08,0.11;-0.08,-0.06;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.SaturateNode;14;-1591.428,-54.25369;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SmoothstepOpNode;15;-1456.552,-60.4026;Inherit;True;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;302;-1197.782,255.4727;Inherit;False;LocalPosY;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;417;-2012.923,931.1544;Inherit;False;Property;_SkinGrowOffset;SkinGrowOffset;20;0;Create;True;0;0;0;False;0;False;0.9;0.85;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;336;-1207.075,-59.7692;Inherit;False;Offset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;413;-1723.262,1074.885;Inherit;False;302;LocalPosY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;418;-1806.161,911.0527;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;412;-1723.782,995.937;Inherit;False;295;LocalPos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;334;-1708.587,509.1169;Inherit;False;295;LocalPos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;411;-1720.769,846.8804;Inherit;False;336;Offset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;303;-1708.067,588.065;Inherit;False;302;LocalPosY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;339;-1705.574,360.0604;Inherit;False;336;Offset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;477;-1489.227,427.0382;Inherit;False;TransformNoise;1;;19;b98c8dae03d1f5d4b8d7339ec2da8a18;0;7;35;FLOAT;0;False;38;FLOAT;0;False;36;FLOAT;0;False;40;FLOAT2;0.7,0;False;46;FLOAT2;0,50;False;42;FLOAT2;0.08,0.47;False;37;FLOAT;0;False;3;FLOAT;0;FLOAT;52;FLOAT;34
Node;AmplifyShaderEditor.FunctionNode;476;-1525.641,919.1629;Inherit;False;TransformNoise;1;;18;b98c8dae03d1f5d4b8d7339ec2da8a18;0;7;35;FLOAT;0;False;38;FLOAT;0;False;36;FLOAT;0;False;40;FLOAT2;0.7,0;False;46;FLOAT2;0,50;False;42;FLOAT2;0.08,0.47;False;37;FLOAT;0;False;3;FLOAT;0;FLOAT;52;FLOAT;34
Node;AmplifyShaderEditor.SmoothstepOpNode;472;-1175.973,918.1006;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;-0.7;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SmoothstepOpNode;469;-1139.757,468.8788;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0.33;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;479;-1207.885,957.4863;Inherit;False;Property;_Float3;Float 3;22;0;Create;True;0;0;0;False;0;False;0;0.4;-0.5;0.5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;473;-1021.826,831.7333;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;471;-992.5373,385.2822;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;49;-1757.898,-283.4813;Inherit;False;Property;_Fresnal;Fresnal;5;0;Create;True;0;0;0;False;0;False;0.02,0.5,3.25;0.09,1.91,3.25;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.StepOpNode;470;-877.2377,487.1437;Inherit;False;2;0;FLOAT;-0.03;False;1;FLOAT;0.04;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;290;161.2533,488.9534;Inherit;False;1626.705;641.8094;Vertex Offset;19;233;264;239;265;269;263;257;245;249;234;299;343;344;373;341;342;422;438;439;;1,1,1,1;0;0
Node;AmplifyShaderEditor.StepOpNode;474;-914.8912,914.7978;Inherit;False;2;0;FLOAT;-0.03;False;1;FLOAT;0.04;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;233;215.0701,858.8205;Inherit;False;1;0;FLOAT;0.2;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;299;289.0409,722.2183;Inherit;False;295;LocalPos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FresnelNode;43;-1518.731,-292.1435;Inherit;True;Standard;WorldNormal;ViewDir;False;False;5;0;FLOAT3;0,0,1;False;4;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;5;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;414;-556.4448,816.4524;Inherit;True;NoiseClip2;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;367;-754.3439,372.211;Inherit;True;NoiseClip;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;329;-1215.261,-293.638;Inherit;False;Fresnel;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;373;887.8906,530.1343;Inherit;False;367;NoiseClip;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;422;878.2541,677.6932;Inherit;False;414;NoiseClip2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;175;382.4739,217.2324;Inherit;False;1;0;FLOAT;0.2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;182;386.6529,290.6178;Inherit;False;Constant;_FlickSpeed;Flick Speed;4;0;Create;True;0;0;0;False;0;False;8;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;264;469.9823,737.8613;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;341;1085.906,698.0773;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;172;603.4604,187.3635;Inherit;True;Simplex2D;True;False;2;0;FLOAT2;1,1;False;1;FLOAT;6.6;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;186;1009.534,257.9565;Inherit;False;Property;_FlickStrenght;Flick Strenght;9;0;Create;True;0;0;0;False;0;False;0.4;0.3;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;239;557.702,935.2687;Inherit;False;Constant;_FlickOffsetDensity;Flick Offset Density;7;0;Create;True;0;0;0;False;0;False;0.58;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;330;1132.409,70.23921;Inherit;False;329;Fresnel;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;458;-1145.495,1109.547;Inherit;False;2;0;FLOAT;0.03;False;1;FLOAT;0.03;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;456;-1163.003,584.7958;Inherit;False;2;0;FLOAT;0.03;False;1;FLOAT;0.03;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;234;610.6179,711.2084;Inherit;True;Simplex2D;True;False;2;0;FLOAT2;2,2;False;1;FLOAT;20;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;439;1069.895,563.6528;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;337;1132.085,153.9651;Inherit;False;336;Offset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;265;663.4811,1014.762;Inherit;False;Constant;_FlickOffsetStrenght;Flick Offset Strenght;8;0;Create;True;0;0;0;False;0;False;0.0045;0.0045;0;0.02;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;269;875.6077,779.3939;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;2.17;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;185;1306.774,190.5968;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;366;1497.078,299.7433;Inherit;False;367;NoiseClip;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SmoothstepOpNode;263;860.3278,881.6779;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0.8;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;327;-983.6069,590.894;Inherit;True;NoiseColor;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;438;1239.966,623.1771;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;415;-979.475,1066.37;Inherit;True;NoiseColor2;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;75;1320.09,95.81249;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;288;1507.114,226.5222;Inherit;False;Property;_Alpha;Alpha;15;0;Create;True;0;0;0;False;0;False;1.85;1.04;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;183;1705.549,111.43;Inherit;False;4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;428;1687.898,327.804;Inherit;False;415;NoiseColor2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;404;1660.512,35.6069;Inherit;False;327;NoiseColor;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;342;1349.558,739.0956;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;257;1062.411,855.736;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;444;1686.292,260.4975;Inherit;False;414;NoiseClip2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;344;1327.708,825.3382;Inherit;False;Property;_FadeStrenght;FadeStrenght;16;0;Create;True;0;0;0;False;0;False;0;-0.08;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;343;1506.62,729.9376;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;400;1874.593,84.22781;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;445;1908.383,287.6596;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;245;1346.431,929.6192;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;200;-1184.585,-1032.03;Inherit;False;1384.681;679.1877;Scroll Pattern;20;226;224;220;227;197;222;218;216;221;194;219;190;225;188;187;192;191;196;223;298;;0,0.1647663,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;291;252.6421,-1266.868;Inherit;False;1931.121;1103.264;Color;27;431;432;405;406;420;430;419;333;403;283;332;171;331;284;202;278;201;277;328;281;287;270;170;271;286;345;282;;0.1861427,0.7370898,0.8396226,1;0;0
Node;AmplifyShaderEditor.LerpOp;427;2107.057,32.04194;Inherit;True;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;249;1644.82,735.4309;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SamplerNode;345;1569.008,-846.4034;Inherit;True;Property;_TextureSample0;Texture Sample 0;17;0;Create;True;0;0;0;False;0;False;-1;None;9870faf3961e38f42b1fb2bf6aeadb9d;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector2Node;225;-617.5093,-600.2492;Inherit;False;Constant;_Scrollfade;Scroll fade;10;0;Create;True;0;0;0;False;0;False;0.83,0.39;0.83,0.39;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.ColorNode;171;438.2655,-1081.497;Inherit;False;Property;_Color0;Color 0;6;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0.8784314,10.2902,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;221;-698.1327,-982.0718;Inherit;False;Property;_Linesnumber;Lines number;12;0;Create;True;0;0;0;False;0;False;1500;1500;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;218;-365.4127,-968.349;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;446;2199.438,-510.3019;Inherit;False;Constant;_Color3;Color 3;21;0;Create;True;0;0;0;False;0;False;1,1,1,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.OneMinusNode;223;-168.7856,-803.6132;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;403;1615.451,-490.3669;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;429;2182.315,269.8135;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.FractNode;194;-448.5577,-774.1286;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;332;912.2794,-419.5128;Inherit;False;329;Fresnel;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;447;2187.438,-639.3019;Inherit;False;1;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;287;277.1392,-483.5107;Inherit;False;Property;_Inverst;Inverst;14;0;Create;True;0;0;0;False;0;False;-0.85;-0.85;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;435;-1953.62,1208.884;Inherit;False;Property;_test;test;18;0;Create;True;0;0;0;False;0;False;0.59,-0.43;0.27,0.07;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.GetLocalVarNode;331;560.8906,-762.7924;Inherit;False;329;Fresnel;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DitheringNode;464;2613.607,21.43265;Inherit;False;1;False;4;0;FLOAT;0;False;1;SAMPLER2D;;False;2;FLOAT4;0,0,0,0;False;3;SAMPLERSTATE;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;191;-721.0895,-756.7302;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCGrayscale;277;967.1927,-1118.225;Inherit;False;0;1;0;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;421;1768.575,389.7986;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;420;1636.059,-375.2218;Inherit;False;414;NoiseClip2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;201;912.0377,-912.9454;Inherit;True;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;475;-754.6131,807.0542;Inherit;True;NoiseClip;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;328;1266.124,-465.5625;Inherit;False;327;NoiseColor;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;224;60.12311,-596.0047;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;190;-914.1862,-608.5125;Inherit;False;Property;_ScrollScale;Scroll Scale;11;0;Create;True;0;0;0;False;0;False;-4.4;-4.4;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;463;2418.607,128.4326;Inherit;False;Constant;_Float2;Float 2;19;0;Create;True;0;0;0;False;0;False;0.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;227;-560.8053,-479.0732;Inherit;False;Constant;_ScrollEdgefade;Scroll Edge fade;11;0;Create;True;0;0;0;False;0;False;0.04,0.8;0.04,0.8;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.SimpleAddOpNode;430;2029.092,-351.754;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;187;-1154.089,-519.7308;Inherit;False;Property;_ScollTimeScale;Scoll Time Scale;10;0;Create;True;0;0;0;False;0;False;0.01081722;0.076;0;0.076;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;431;1639.793,-294.5467;Inherit;False;415;NoiseColor2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;216;-508.2206,-967.3909;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;70;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;222;-232.738,-967.4254;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;448;2444.438,-250.3019;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.Vector2Node;442;-1945.492,1077.233;Inherit;False;Property;_test2;test2;19;0;Create;True;0;0;0;False;0;False;0.59,-0.43;0.01,0.57;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RangedFloatNode;406;1266.187,-374.4105;Inherit;False;Constant;_Float0;Float 0;18;0;Create;True;0;0;0;False;0;False;0.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;192;-568.5883,-717.3599;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;3.15;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;283;916.3966,-681.9501;Inherit;False;Property;_Color2;Color 2;8;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0.4735315,0.02415763,0.1714412,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;170;446.4994,-582.0221;Inherit;True;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SmoothstepOpNode;196;-321.5211,-793.1952;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;188;-868.5255,-513.6897;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;282;983.2836,-1216.868;Inherit;False;Constant;_GridStrenght;Grid Strenght;14;0;Create;True;0;0;0;False;0;False;0.25;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;443;1953.121,0.0586853;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;226;-130.9338,-502.168;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;419;1798.553,-528.5783;Inherit;True;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;278;704.9929,-591.3387;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;333;271.6708,-614.1279;Inherit;False;329;Fresnel;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;449;2420.438,-598.3019;Inherit;False;Constant;_Float1;Float 1;21;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;271;430.8058,-1201.705;Inherit;False;0;270;2;3;2;SAMPLER2D;;False;0;FLOAT2;4,4;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TexturePropertyNode;468;2333.607,-96.56735;Inherit;True;Property;_Texture0;Texture 0;21;0;Create;True;0;0;0;False;0;False;None;a5f44c8e2925639418c346be36ef777b;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.GetLocalVarNode;298;-923.1183,-865.6565;Inherit;False;295;LocalPos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;405;1493.907,-386.984;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;281;1171.632,-1137.696;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;219;-716.7347,-885.9315;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SmoothstepOpNode;197;-306.3925,-514.1539;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1.33;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;284;1202.532,-783.6871;Inherit;True;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;220;-21.06856,-844.726;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;286;1070.591,-519.8068;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;28.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;202;450.3545,-918.0053;Inherit;False;Property;_Color1;Color 1;7;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0.1254902,14.68235,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;270;654.4397,-1206.827;Inherit;True;Property;_Grid;Grid;13;1;[HideInInspector];Create;True;0;0;0;False;0;False;-1;a8fded81109f863439c40a4247ce26a3;a8fded81109f863439c40a4247ce26a3;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;432;1832.176,-206.0093;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;8;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;9;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;-1697.083,-383.2565;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;2820.779,17.91309;Float;False;True;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;SCS/VFX/TransparentArm;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;False;0;Hidden/InternalErrorShader;0;0;Standard;22;Surface;1;  Blend;0;Two Sided;1;Cast Shadows;1;  Use Shadow Threshold;0;Receive Shadows;1;GPU Instancing;1;LOD CrossFade;0;Built-in Fog;0;DOTS Instancing;0;Meta Pass;0;Extra Pre Pass;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;5;False;True;True;True;False;False;;False;0
WireConnection;295;0;296;1
WireConnection;450;0;410;0
WireConnection;12;0;297;0
WireConnection;12;1;450;0
WireConnection;14;0;12;0
WireConnection;15;0;14;0
WireConnection;15;1;16;1
WireConnection;15;2;16;2
WireConnection;302;0;296;2
WireConnection;336;0;15;0
WireConnection;418;0;450;0
WireConnection;418;1;417;0
WireConnection;477;35;339;0
WireConnection;477;38;450;0
WireConnection;477;36;334;0
WireConnection;477;37;303;0
WireConnection;476;35;411;0
WireConnection;476;38;418;0
WireConnection;476;36;412;0
WireConnection;476;37;413;0
WireConnection;472;0;476;52
WireConnection;469;0;477;52
WireConnection;473;0;472;0
WireConnection;473;1;476;0
WireConnection;471;0;469;0
WireConnection;471;1;477;0
WireConnection;470;1;471;0
WireConnection;474;0;479;0
WireConnection;474;1;473;0
WireConnection;43;1;49;1
WireConnection;43;2;49;2
WireConnection;43;3;49;3
WireConnection;414;0;474;0
WireConnection;367;0;470;0
WireConnection;329;0;43;0
WireConnection;264;0;299;0
WireConnection;264;1;233;0
WireConnection;341;0;422;0
WireConnection;172;0;175;0
WireConnection;172;1;182;0
WireConnection;458;1;476;34
WireConnection;456;1;477;34
WireConnection;234;0;264;0
WireConnection;439;0;373;0
WireConnection;269;0;172;0
WireConnection;185;0;172;0
WireConnection;185;1;186;0
WireConnection;263;0;234;0
WireConnection;263;1;239;0
WireConnection;327;0;456;0
WireConnection;438;0;439;0
WireConnection;438;1;341;0
WireConnection;415;0;458;0
WireConnection;75;0;330;0
WireConnection;75;1;337;0
WireConnection;183;0;75;0
WireConnection;183;1;185;0
WireConnection;183;2;288;0
WireConnection;183;3;366;0
WireConnection;342;0;438;0
WireConnection;257;0;269;0
WireConnection;257;1;263;0
WireConnection;257;2;265;0
WireConnection;343;0;342;0
WireConnection;343;1;344;0
WireConnection;400;0;404;0
WireConnection;400;1;183;0
WireConnection;445;0;444;0
WireConnection;445;1;428;0
WireConnection;245;0;257;0
WireConnection;427;0;400;0
WireConnection;427;2;445;0
WireConnection;249;0;343;0
WireConnection;249;1;245;0
WireConnection;218;0;216;0
WireConnection;223;0;196;0
WireConnection;403;0;284;0
WireConnection;403;1;405;0
WireConnection;429;0;249;0
WireConnection;429;2;445;0
WireConnection;194;0;192;0
WireConnection;464;0;427;0
WireConnection;191;0;298;0
WireConnection;191;1;188;0
WireConnection;277;0;270;0
WireConnection;201;0;202;0
WireConnection;201;1;171;0
WireConnection;201;2;278;0
WireConnection;475;0;474;0
WireConnection;224;0;220;0
WireConnection;224;1;226;0
WireConnection;430;0;419;0
WireConnection;430;1;431;0
WireConnection;216;0;219;0
WireConnection;216;1;221;0
WireConnection;222;0;218;0
WireConnection;448;0;430;0
WireConnection;448;1;446;0
WireConnection;448;2;449;0
WireConnection;192;0;191;0
WireConnection;192;1;190;0
WireConnection;170;0;333;0
WireConnection;170;1;224;0
WireConnection;170;2;287;0
WireConnection;196;0;194;0
WireConnection;196;1;225;1
WireConnection;196;2;225;2
WireConnection;188;0;187;0
WireConnection;226;0;197;0
WireConnection;419;0;403;0
WireConnection;419;1;345;0
WireConnection;419;2;420;0
WireConnection;278;0;281;0
WireConnection;278;1;170;0
WireConnection;405;0;328;0
WireConnection;405;1;406;0
WireConnection;281;0;282;0
WireConnection;281;1;277;0
WireConnection;219;0;298;0
WireConnection;219;1;188;0
WireConnection;197;0;194;0
WireConnection;197;1;227;1
WireConnection;197;2;227;2
WireConnection;284;0;283;0
WireConnection;284;1;201;0
WireConnection;284;2;286;0
WireConnection;220;0;222;0
WireConnection;220;1;223;0
WireConnection;286;0;332;0
WireConnection;270;1;271;0
WireConnection;432;0;431;0
WireConnection;6;2;448;0
WireConnection;6;3;464;0
WireConnection;6;4;463;0
WireConnection;6;5;429;0
ASEEND*/
//CHKSM=69AD7D5B7E28303B9E4C6D0711270C2F45C424D5