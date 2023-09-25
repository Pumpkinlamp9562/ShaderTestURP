using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class rotator : MonoBehaviour
{
    [SerializeField]
    float speed;
    [SerializeField]
    bool x;
    [SerializeField]
    bool y;
    [SerializeField]
    bool z;
    // Update is called once per frame
    void Update()
    {
        Vector3 dir = x ? Vector3.right : y ? Vector3.up : z ? Vector3.forward : Vector3.zero;
        transform.Rotate(dir * Time.deltaTime* speed);      
    }
}
