using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[ExecuteAlways]
public class GrowControl : MonoBehaviour
{
    [SerializeField] Material renderer;
    [SerializeField] Material renderer2;
    [SerializeField, Range(0,1)] float grow;

	private void OnValidate()
	{
		if (renderer == null)
			return;
		renderer.SetFloat("_transform", grow +1);
		renderer2.SetFloat("_transform", grow +1);
	}
}
