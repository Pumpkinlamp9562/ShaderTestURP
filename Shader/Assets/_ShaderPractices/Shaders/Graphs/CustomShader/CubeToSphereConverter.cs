#if UNITY_EDITOR
using UnityEngine;
using UnityEngine.Rendering;
using System.IO;
using UnityEditor;
using System.Collections.Generic;
using SCS.Development.Diagnostics;
public class CubemapConverter : MonoBehaviour
{
	public Texture2D Convert6FacesToCubic(Dictionary<string, Texture2D> outputCubeMap, int outputWidth, int outputHeight)
	{
		string[] cubeFaceNames = { "_positiveX", "_positiveY", "_positiveZ", "_negativeX", "_negativeY", "_negativeZ" };

		// Create the output texture
		Texture2D outputTexture = new Texture2D(outputWidth, outputHeight, TextureFormat.RGBA32, true);

		for (int i = 0; i < cubeFaceNames.Length; i++)
		{
			// Load the cube face texture
			Texture2D cubeFaceTexture = outputCubeMap[cubeFaceNames[i]];
			try { int d = cubeFaceTexture.width; }
			catch { SCSLogger.Debug("Can't find " + cubeFaceNames[i]); return null; }
			// Flip the cube face texture
			//cubeFaceTexture = RotateTexture(cubeFaceTexture);
			//cubeFaceTexture = RotateTexture(cubeFaceTexture);

			// Calculate the sub-image position
			int subImageWidth = outputTexture.width / 4;
			int subImageHeight = outputTexture.height / 3;
			int subImageX = 0;
			int subImageY = 0;
			switch (cubeFaceNames[i])
			{
				case "_positiveY":
					subImageX = subImageWidth * 1;
					subImageY = subImageHeight * 2;
					break;
				case "_positiveX":
					subImageX = subImageWidth * 0;
					subImageY = subImageHeight * 1;
					break;
				case "_positiveZ":
					subImageX = subImageWidth * 1;
					subImageY = subImageHeight * 1;
					break;
				case "_negativeX":
					subImageX = subImageWidth * 2;
					subImageY = subImageHeight * 1;
					break;
				case "_negativeZ":
					subImageX = subImageWidth * 3;
					subImageY = subImageHeight * 1;
					break;
				case "_negativeY":
					subImageX = subImageWidth * 1;
					subImageY = subImageHeight * 0;
					break;
			}


			// Copy the cube face texture to the output texture
			for (int x = 0; x < subImageWidth; x++)
			{
				for (int y = 0; y < subImageHeight; y++)
				{
					Color pixel = cubeFaceTexture.GetPixel(x, y);
					outputTexture.SetPixel(subImageX + x, subImageY + y, pixel);
				}
			}

			// Destroy the cube face texture to release memory
			File.Delete(Application.dataPath.Substring(0, Application.dataPath.Length - 6) + AssetDatabase.GetAssetPath(outputCubeMap[cubeFaceNames[i]]));
			AssetDatabase.Refresh();
		}

		// Apply the changes to the output texture
		outputTexture.Apply();
		return outputTexture;
	}

	public Texture2D CubicToSphere(Cubemap cubemap)
	{
		SCSLogger.Debug(cubemap.width*2 + "/" + cubemap.height, LOGCATEGORY.DEFAULT);
		RenderTexture newCubemap = RenderTexture.GetTemporary(cubemap.width * 2, cubemap.height, 0, RenderTextureFormat.Default, RenderTextureReadWrite.Linear);
		RenderTexture previous = RenderTexture.active;
		RenderTexture tmp = new RenderTexture(cubemap.width , cubemap.height, 24);
		tmp.antiAliasing = 8;
		tmp.dimension = TextureDimension.Cube;
		tmp.Create();
		// Put the cubemap on the tmp
		//		Graphics.Blit(cubemap, tmp);

		// Create a temporary camera
		GameObject camera = new GameObject("TmpCamera");
		camera.AddComponent<Camera>();
		camera.GetComponent<Camera>().RenderToCubemap(tmp);
		//Unity wiil change the RenderToCubemap(tmp) to cubic, which will match the cubemap(Texture)
		for (int i = 0; i< 6; i++)
		{
			Graphics.CopyTexture(cubemap, i, 0, tmp, i,0);
		}
		RenderTexture.active = tmp;
		// Change the tmp to equirect, because tmp is create by RenderToCubemap(), so it will match the requit from ConvertToEquirect, so it will render all the faces this time
		tmp.ConvertToEquirect(newCubemap, Camera.MonoOrStereoscopicEye.Mono);
		RenderTexture.active = newCubemap;

		// Get the newest active render texture
		Texture2D tex = new Texture2D(cubemap.width * 2, cubemap.height, TextureFormat.RGBA32, mipChain: true);
		tex.ReadPixels(new Rect(0, 0, cubemap.width * 2, cubemap.height), 0, 0);
		tex = MaterialConverter.HDRtoRGB(tex);
		tex.Apply();

		RenderTexture.active = previous;

		DestroyImmediate(camera);
		return tex;
	}

	private Texture2D RotateTexture(Texture2D texture)
	{
		Texture2D rotatedTexture = new Texture2D(texture.height, texture.width);
		for (int x = 0; x < texture.width; x++)
		{
			for (int y = 0; y < texture.height; y++)
			{
				Color pixel = texture.GetPixel(x, y);
				rotatedTexture.SetPixel(y, texture.width - 1 - x, pixel);
			}
		}
		rotatedTexture.Apply();
		return rotatedTexture;
	}
}
#endif