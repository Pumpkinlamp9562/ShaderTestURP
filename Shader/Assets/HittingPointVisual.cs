using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;
[ExecuteInEditMode]
public class HittingPointVisual : MonoBehaviour
{
    [SerializeField] Transform hitPoint;
    [SerializeField] Renderer renderer;

    // Update is called once per frame
    void Update()
    {
        renderer.material.SetVector("_HitPoint", hitPoint.localPosition);
    }
}
