using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FireFollowEffect : MonoBehaviour
{
    Transform lowArmJnt;
    Vector3 startPos;
    ParticleSystem ps;
    [SerializeField] Transform endPos;
    [SerializeField] Vector2 MinMax;
    [SerializeField] Material mat;
    [SerializeField] Material fire;

	private void Start()
	{
        startPos = transform.position;
        ps = GetComponent<ParticleSystem>();
    }
	// Update is called once per frame
	void Update()
    {
        CaculateParticlePos();
    }

    void CaculateParticlePos()
	{
        if (mat.GetFloat("_Offset") > 0 && mat.GetFloat("_Offset") < 1 && !ps.isPlaying)
		{
            ps.Play();
            var emission = ps.emission;
            emission.enabled = true;
        }

        if ((mat.GetFloat("_Offset") == 1 || mat.GetFloat("_Offset") == 0) && ps.isPlaying)
		{
            var emission = ps.emission;
            emission.enabled = false;
            ps.Stop();
        }


        transform.position = Vector3.Lerp(startPos, endPos.position, mat.GetFloat("_Offset"));

            fire.SetFloat("_Alpha", mat.GetFloat("_Offset")* mat.GetFloat("_Offset"));
    }
}
