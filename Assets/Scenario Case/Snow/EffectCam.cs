using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EffectCam : MonoBehaviour
{
    public RenderTexture rt;

    public Transform target;
    // Start is called before the first frame update
    void Start()
    {
        Shader.SetGlobalTexture("_GlobalEffectRT",rt);
        Shader.SetGlobalFloat("_OrthographicCamSize",GetComponent<Camera>().orthographicSize);
    }

    // Update is called once per frame
    void Update()
    {
        transform.position =
            new Vector3(target.transform.position.x, transform.position.y, target.transform.position.z);
        Shader.SetGlobalVector("_Position" , transform.position);
    }
}
