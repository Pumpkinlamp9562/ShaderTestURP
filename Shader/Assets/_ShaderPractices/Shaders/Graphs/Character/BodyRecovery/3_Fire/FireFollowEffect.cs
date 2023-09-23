using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NaughtyAttributes;

public class FireFollowEffect : MonoBehaviour
{
    Transform lowArmJnt;
    Vector3 startPos;
    ParticleSystem ps;
    [SerializeField] Vector3 endPos;
    [SerializeField] Vector2 MinMax;
    [SerializeField] Material mat;
    [SerializeField] Material fire;
	//mdk_R_lowarm_jnt

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

    [Button]
    void CaculateJointPos()
	{
        mat.SetVector("_LocalPos", lowArmJnt.position);
        mat.SetVector("_Forward", lowArmJnt.right);
	}

    [Button]
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


        transform.position = Vector3.Lerp(startPos, endPos + startPos, mat.GetFloat("_Offset"));

        if(fire.HasFloat("_Alpha"))
            fire.SetFloat("_Alpha", Mathf.Max(Mathf.Lerp(MinMax.x, MinMax.y, mat.GetFloat("_Offset")),1));
    }
}
