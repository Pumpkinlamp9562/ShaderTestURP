using NaughtyAttributes;
using UnityEngine;

public class OpponentMaterialsDither : MonoBehaviour
{
    [SerializeField] private Renderer[] renderers;
    [SerializeField] private Shader dither;
    [SerializeField, Range(0, 1)] private float alpha;
    [SerializeField] private Material m_dither;

    [Button]
    private void UpdateMaterialsAlphaToDither()
    {
        m_dither = new Material(dither); //TODO - dont creat a new material every time
        m_dither.SetFloat("_Dither", alpha);

        for (int i = 0; i < renderers.Length; i++)
        {
            Color oldColor = renderers[i].material.GetColor("_BaseColor");
            Color newColor = new Color(oldColor.r, oldColor.g, oldColor.b, m_dither.GetFloat("_Alpha"));
            renderers[i].material.SetColor("_BaseColor", newColor);
        }
    }
}