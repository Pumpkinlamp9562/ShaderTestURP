using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using Cysharp.Threading.Tasks;
using System.Threading;
using DG.Tweening;

public class BloodTensionControl : MonoBehaviour
{
    CancellationTokenSource cancellationSource;
    Renderer renderer;
    int target = 2;
    void Start()
    {
        TryGetComponent<Renderer>(out renderer);
    }

    
    //Set the material of the blood in the goblet
    public void SetTheheightOfTheBlood(bool tileIn)
    {
        cancellationSource = cancellationSource.SafeReset();
        SurfaceTension(tileIn, cancellationSource.Token).Forget();
    }

    private async UniTask SurfaceTension(bool tileIn, CancellationToken token)
    {
        MaterialPropertyBlock mpb = new MaterialPropertyBlock();
        int t = tileIn ? 1 : 0;
        if (target == t)
            return;
        target = t;
        await DOVirtual.Float(renderer.sharedMaterial.GetFloat("_VertexHeightControl"), target, 1f, v => { mpb.SetFloat("_VertexHeightControl", v); renderer.SetPropertyBlock(mpb); }).ToUniTask(cancellationToken: token);
    }
}
