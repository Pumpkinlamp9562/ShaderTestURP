using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using UnityEngine.UIElements;

enum Culling
{
    BOTH,
    FRONT,
    BACK
}


public class SCSMaterialSetting : MonoBehaviour
{
    [SerializeField] Texture texture;
    [SerializeField] Color color = Color.white;
    [SerializeField] Culling culling = Culling.FRONT;

    void OnValidate()
    {
        if (TryGetComponent<Renderer>(out Renderer r))
            r.SetPropertyBlock(SetProperty());
            
    }

    MaterialPropertyBlock SetProperty()
	{
        MaterialPropertyBlock mpb = new MaterialPropertyBlock();
        mpb.SetColor("_Color", color);
        mpb.SetColor("_BaseColor", color);
        mpb.SetFloat("_Culling", (float)((int)culling));
        if(texture != null)
        mpb.SetTexture("_MainTex", texture);
        return mpb;
    }

    public void SetColor(Color newColor)
    {
        color = newColor;
        if (TryGetComponent<Renderer>(out Renderer r))
            r.SetPropertyBlock(SetProperty());
    }
}