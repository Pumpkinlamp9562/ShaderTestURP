//References:
//https://github.com/maxartz15/MA_TextureUtils
//https://docs.unity3d.com/2020.1/Documentation/ScriptReference/ImageConversion.EncodeToEXR.html

#if UNITY_EDITOR
using System.Collections;
using System;
using System.IO;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;
using NaughtyAttributes;
using SCS.Development.Diagnostics;

public class MaterialConverter : MonoBehaviour
{
    [SerializeField]
    Material previousMat;
    [SerializeField]
    Texture reflectionMap;
    [SerializeField]
    bool flipCubePositiveZ;

    int textureSize = 512; //TODO-Scaling
    Shader scsSimplePBR;
    string path;
    Texture2D currentMetallicGlossMap;
    Texture2D currentBaseMap;
    Texture2D currentNormalMap;
    Texture2D newBaseMap;
    Cubemap newReflectionMap;
    CubemapConverter cubeToSphereConverter;
    Dictionary<string, Texture2D> outputCubeMap = new Dictionary<string, Texture2D>();

    [Button]
    public void Convert()
    {
        cubeToSphereConverter = new CubemapConverter();
        // Get the new shader
        try { Shader.Find("SCS/SimplePBR"); }
        catch { SCSLogger.Debug("Can't find SCS/SimplePBR"); return; }
        scsSimplePBR = Shader.Find("SCS/SimplePBR");
        // Get the previous material
        try { GetComponent<Renderer>(); }
        catch { SCSLogger.Debug("Can't get renderer on this object"); return; }
        Renderer renderer = GetComponent<Renderer>();
        if (renderer.sharedMaterial.shader != scsSimplePBR)
        {
            previousMat = renderer.sharedMaterial;
        }
        
        // Create path
        try { path = AssetDatabase.GetAssetPath(previousMat); }
        catch
        { SCSLogger.Debug("Previous Material is missing"); return; }

        path = path.Substring(0, path.LastIndexOf("/"));

        // Get texture
        try { previousMat.GetTexture("_MetallicGlossMap"); }
        catch
        { SCSLogger.Debug("Previous Material _MetallicGlossMap is missing"); return; }

        textureSize = previousMat.GetTexture("_MetallicGlossMap").width;
        currentMetallicGlossMap = (Texture2D)ConvertToReadableTexture(previousMat.GetTexture("_MetallicGlossMap"), "_Metallic", false) ;
        currentBaseMap = (Texture2D)ConvertToReadableTexture(previousMat.GetTexture("_BaseMap"), "_BaseMap", true);
        currentNormalMap = (Texture2D)ConvertToReadableTexture(previousMat.GetTexture("_BumpMap"), "_BumpMap", false);

        newBaseMap = currentBaseMap;
        newReflectionMap = (Cubemap)reflectionMap;

        // Get the color
        Color[] metallicGlossCol = currentMetallicGlossMap.GetPixels();
        AssetDatabase.Refresh();

        Color[] baseMapCol = currentBaseMap.GetPixels();
        Color[] bumpMapCol = currentNormalMap.GetPixels();
        
        // Set up the textures in new material
        Material scsMaterial = new Material(scsSimplePBR);
        scsMaterial.SetTexture("_MainTex", NewBaseMap(metallicGlossCol, baseMapCol, bumpMapCol));
        scsMaterial.SetTexture("_ReflectTex", NewReflectionMap(metallicGlossCol, bumpMapCol));
        renderer.sharedMaterial = SavedMaterialData(scsMaterial);
        File.Delete(Application.dataPath.Substring(0, Application.dataPath.Length - 6) + path + "/t_" + gameObject.name + "_Metallic.png");
    }
    #region ChangePixelsColor
    Texture2D NewBaseMap(Color[] glossCol, Color[] baseCol, Color[] bumpCol)
    {
        List<Color> outputCol = new List<Color>();

        for (int i = 0; i < baseCol.Length; i++)
        {
            outputCol.Add(new Color(baseCol[i].r, baseCol[i].g, baseCol[i].b, glossCol[i].r + (1-bumpCol[i].g)));
        }
        newBaseMap.SetPixels(outputCol.ToArray());
        newBaseMap.Apply();
        return SaveImage(newBaseMap, "_BaseMap");
    }

    Cubemap NewReflectionMap(Color[] matallicCol, Color[] bumpCol)
    {
        // Get the new cloned cube map texture from ConvertToReadableCubemap
        Cubemap ReflectMap = ConvertToReadableCubemap(reflectionMap, "_Reflection");

        // Get the pixels from new cloned cube map texture, and merge it with metallic alpha
        Color[] reflectionCol = ReflectMap.GetPixels(CubemapFace.PositiveZ, 0);
        List<Color> outputCol = new List<Color>();


        if(matallicCol.Length != reflectionCol.Length)
        {
            SCSLogger.Debug("Scale the texture: from " + currentMetallicGlossMap.width + " x " + currentMetallicGlossMap.height + " to: " 
                +ReflectMap.width + " x " + ReflectMap.height);
            matallicCol = Scale(matallicCol, currentMetallicGlossMap.width, currentMetallicGlossMap.height, ReflectMap.width, ReflectMap.width);
            SCSLogger.Debug(reflectionCol.Length, LOGCATEGORY.DEFAULT);
            SCSLogger.Debug(matallicCol.Length, LOGCATEGORY.DEFAULT);
        }

        for (int i = matallicCol.Length-1; i >= 0; i--)
        {
                outputCol.Add(new Color(reflectionCol[i].r, reflectionCol[i].g, reflectionCol[i].b, matallicCol[i].a + bumpCol[i].r));
        }
        newReflectionMap.SetPixels(outputCol.ToArray(), CubemapFace.PositiveZ);
        newReflectionMap.Apply();

        // Save as 6 faces file
        outputCubeMap.Clear();
        outputCubeMap.Add("_positiveZ", SaveCubemap(outputCol.ToArray(), "_positiveZ", flipCubePositiveZ));
        outputCubeMap.Add("_positiveX", SaveCubemap(newReflectionMap.GetPixels(CubemapFace.PositiveX), "_positiveX", true));
        outputCubeMap.Add("_positiveY", SaveCubemap(newReflectionMap.GetPixels(CubemapFace.PositiveY), "_positiveY", true));
        outputCubeMap.Add("_negativeX", SaveCubemap(newReflectionMap.GetPixels(CubemapFace.NegativeX), "_negativeX", true));
        outputCubeMap.Add("_negativeY", SaveCubemap(newReflectionMap.GetPixels(CubemapFace.NegativeY), "_negativeY", true));
        outputCubeMap.Add("_negativeZ", SaveCubemap(newReflectionMap.GetPixels(CubemapFace.NegativeZ), "_negativeZ", true));

        // Convert the cubemap from 6 faces to cubic
        int cubemapFaceWidth = (int)Math.Ceiling(Mathf.Sqrt(outputCol.Count));
        Texture2D cubic = cubeToSphereConverter.Convert6FacesToCubic(outputCubeMap, cubemapFaceWidth * 4, cubemapFaceWidth * 3);
        SaveImage(cubic, "_ReflectionCubic");

        // Convert the cubemap from cubic to sphere
        SaveImage(cubeToSphereConverter.CubicToSphere(newReflectionMap), "_Reflection");

        // Set it to TextureCube 
        TextureSetting(path + "/t_" + gameObject.name + "_Reflection.png", TextureImporterFormat.RGBA32, TextureImporterShape.TextureCube, textureSize * 2);

        // Delete _ReflectionCubic
        File.Delete(Application.dataPath.Substring(0, Application.dataPath.Length - 6) + path + "/t_" + gameObject.name + "_ReflectionCubic.png");
        AssetDatabase.Refresh();
        return newReflectionMap;
    }
    #endregion

    #region ConvertToReadableTexture
    /// <summary>
    /// Clone the texture and change it to a readable texture with target format
    /// </summary>
    /// <param name="texture"></param>
    /// <returns></returns>
    public Texture ConvertToReadableTexture(Texture texture, string name, bool HDR)
    {
        if (texture == null) return null;

        // Encode texture into the png
        
            Texture2D tex = new Texture2D(textureSize, textureSize);
        if (HDR)
            tex = HDRtoRGB(CreateCloneFromRenderTexture(texture, RenderTextureFormat.Default));
        else
            tex = CreateCloneFromRenderTexture(texture, RenderTextureFormat.Default);
            
        return SaveImage(tex, name);
    }

    /// <summary>
    /// Clone the texture and change it to a readable texture with target format
    /// </summary>
    /// <param name="texture"></param>
    /// <param name="tgaOrpng">true = TGA, false = PNG</param>
    /// <returns></returns>
    public Cubemap ConvertToReadableCubemap(Texture texture, string name)
    {
        if (texture == null) return null;

        // Change target texture from cube map to texture2D
        string copytargetCubePath = AssetDatabase.GetAssetPath(texture);
        TextureSetting(copytargetCubePath, TextureImporterFormat.RGBAHalf, TextureImporterShape.Texture2D, textureSize*2);
        texture = AssetDatabase.LoadAssetAtPath<Texture>(copytargetCubePath);

        // Change the texture to be readable, change it from HDR to RGB, Encode texture into the png
        byte[] bytes = ImageConversion.EncodeToPNG(HDRtoRGB(CreateCloneFromRenderTexture(texture, RenderTextureFormat.DefaultHDR)));
        File.WriteAllBytes(path + "/" + "t_" + gameObject.name + name + ".png", bytes);
        AssetDatabase.SaveAssets();
        AssetDatabase.Refresh();

        // Change target texture back to cube map
            // TODO - If the texture is .png or .tga it will be (with Alpha) RGBA32, (without Alpha) RGB24
            // If the texture is .jpg, the texture format will be RGB24
            // If the texture is .Exr, the texture format will be RGBAHalf
        TextureSetting(copytargetCubePath, TextureImporterFormat.RGBAHalf, TextureImporterShape.TextureCube, textureSize*2);
        AssetDatabase.SaveAssets();
        AssetDatabase.Refresh();

        // Change the new cloned texture to cubemap and output the texture
        TextureSetting(path + "/t_" + gameObject.name + name + ".png", TextureImporterFormat.RGBA32, TextureImporterShape.TextureCube, textureSize*2);
        Texture output = AssetDatabase.LoadAssetAtPath<Texture>(path + "/" + "t_" + gameObject.name + name + ".png");
        EditorUtility.SetDirty(output);
        AssetDatabase.SaveAssetIfDirty(output);
        newReflectionMap = (Cubemap)output;

        return newReflectionMap;
    }

    /// <summary>
    /// Chaage a HDR color to sRGB (Linear to Gamma0.45)
    /// </summary>
    /// <param name="copytargetTex2D"></param>
    /// <returns></returns>
    public static Texture2D HDRtoRGB(Texture2D copytargetTex2D)
    {
        //Convert to sRGB
        Texture2D copytargetTex2DRGB = new Texture2D(copytargetTex2D.width, copytargetTex2D.height);
        copytargetTex2DRGB.SetPixels(copytargetTex2D.GetPixels());
        copytargetTex2DRGB.Apply();
        Color[] Pixel = copytargetTex2DRGB.GetPixels();

        for (int i = 0; i < Pixel.Length; ++i)
        {
            Color Rgb = new Color();

            for (int j = 0; j < 4; ++j)
            {
                Rgb[j] = Mathf.LinearToGammaSpace(Pixel[i][j]);
            }

            Pixel[i] = Rgb;
        }
        copytargetTex2DRGB.SetPixels(Pixel);
        copytargetTex2DRGB.Apply();
        return copytargetTex2DRGB;
    }

    /// <summary>
    /// Convert a texture to a readable file 
    /// </summary>
    /// <param name="texture"></param>
    /// <param name="format"></param>
    /// <returns></returns>
    Texture2D CreateCloneFromRenderTexture(Texture texture, RenderTextureFormat format)
    {
        // Create a temporary RenderTexture with the same dimensions as the texture
        RenderTexture tmp = RenderTexture.GetTemporary(texture.width, texture.height, 0, format, RenderTextureReadWrite.Linear);

        // Backup the currently set RenderTexture
        RenderTexture previous = RenderTexture.active;

        // Set the temporary RenderTexture as the active RenderTexture
        RenderTexture.active = tmp;

        // Create a new Texture with the same dimensions as the original texture
        Texture2D tex = new Texture2D(texture.width, texture.height, TextureFormat.RGBAHalf, mipChain: true);

        // Copy the pixels from the original texture to the temporary RenderTexture
        Graphics.Blit(texture, tmp);

        // Copy the pixels from the temporary RenderTexture to the new Cubemap
        tex.ReadPixels(new Rect(0, 0, texture.width, texture.height), 0, 0);
        tex.Apply();

        // Reset the active RenderTexture
        RenderTexture.active = previous;
        // Release the temporary RenderTexture
        RenderTexture.ReleaseTemporary(tmp);

        return tex;
    }

    void TextureSetting(string path, TextureImporterFormat format, TextureImporterShape shape, int size)
    {

        var tImporter = AssetImporter.GetAtPath(path) as TextureImporter;
        if (tImporter != null)
        {
            tImporter.textureType = TextureImporterType.Default;
            tImporter.textureShape = shape;
            tImporter.isReadable = true;

            AssetDatabase.ImportAsset(path);
            EditorUtility.SetDirty(tImporter);
            tImporter.SaveAndReimport();
            AssetDatabase.SaveAssets();
            AssetDatabase.Refresh();
        }

        // Settings Android.
        TextureImporterPlatformSettings android_settings = tImporter.GetDefaultPlatformTextureSettings();
        android_settings.overridden = true;
        android_settings.name = "Android";
        android_settings.maxTextureSize = size;
        android_settings.format = format;
        tImporter.SetPlatformTextureSettings(android_settings);
        EditorUtility.SetDirty(tImporter);
        tImporter.SaveAndReimport();
        AssetDatabase.SaveAssets();
        AssetDatabase.Refresh();
    }
    #endregion

    #region TextureScaling

    Color[] Scale(Color[] currentColors, int currentWidth, int currentHeight, int newWidth, int newHeight)
    {
        Color[] newColors = new Color[newWidth * newHeight];

        float ratioX = 1.0f / ((float)newWidth / (currentWidth - 1));
        float ratioY = 1.0f / ((float)newHeight / (currentHeight - 1));

        for (int y = 0; y < newHeight; y++)
        {
            int yFloor = Mathf.FloorToInt(y * ratioY);
            var y1 = (yFloor + 1) * currentWidth;
            var yw = y * newWidth;

            for (int x = 0; x < newWidth; x++)
            {
                int xFloor = Mathf.FloorToInt(x * ratioX);

                newColors[yw + x] = currentColors[Mathf.RoundToInt(y1 + xFloor)];
            }
        }

        return newColors;
    }

    #endregion

    #region Saving
    Material SavedMaterialData(Material scsMaterial)
    {
        AssetDatabase.CreateAsset(scsMaterial, path + "/" + "m_" + gameObject.name + ".mat");
        EditorUtility.SetDirty(scsMaterial);
        AssetDatabase.SaveAssetIfDirty(scsMaterial);
        return scsMaterial;
    }

    Texture2D SaveCubemap(Color[] colors, string name, bool revertPixel)
    {
        List<Color> outputCol = new List<Color>();
        if (revertPixel)
        {
            for (int i = colors.Length - 1; i >= 0; i--)
            {
                outputCol.Add(new Color(colors[i].r, colors[i].g, colors[i].b, 1));
            }
        }
        else
        {
            outputCol.AddRange(colors);
        }

        Texture2D face = new Texture2D((int)Math.Ceiling(Mathf.Sqrt(outputCol.Count)), (int)Math.Ceiling(Mathf.Sqrt(outputCol.Count)));
        face.SetPixels(outputCol.ToArray());
        face.Apply();
        return SaveImage(face, name);
    }

    Texture2D SaveImage(Texture2D texture, string name)
    {
        try { AssetDatabase.LoadAssetAtPath<UnityEngine.Object>(path + "/" + "t_" + gameObject.name + name + ".png"); }
        catch { SCSLogger.Debug("Can't find " + path + "/" + "t_" + gameObject.name + name + ".png", LOGCATEGORY.ERROR); }
        byte[] bytes = ImageConversion.EncodeToPNG(texture);
        File.WriteAllBytes(path + "/" + "t_" + gameObject.name + name + ".png", bytes);
        AssetDatabase.SaveAssets();
        AssetDatabase.Refresh();

        TextureSetting(path + "/" + "t_" + gameObject.name + name + ".png", TextureImporterFormat.RGBA32, TextureImporterShape.Texture2D, textureSize);
        Texture2D output = (Texture2D)AssetDatabase.LoadAssetAtPath<Texture>(path + "/" + "t_" + gameObject.name + name + ".png");

        EditorUtility.SetDirty(output);
        AssetDatabase.SaveAssetIfDirty(output);
        return output;
    }
    #endregion
}
#endif