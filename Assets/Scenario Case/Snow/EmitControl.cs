using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.UIElements;

public class EmitControl : MonoBehaviour
{
    private ParticleSystem _particleSystem;

    
    private ParticleSystem.EmitParams _params;

    private Vector3 originDir;

    private float timeCounter;
    // Start is called before the first frame update
    void Start()
    {
        _particleSystem = GetComponent<ParticleSystem>();
        _params = new ParticleSystem.EmitParams();
        originDir = new Vector3(0, 0, 1);
        timeCounter = 0;
    }

    // Update is called once per frame
    void Update()
    {
        timeCounter += Time.deltaTime;
        Shader.SetGlobalVector("_CharacterPos",transform.position);
    }

    private void OnTriggerEnter(Collider other)
    {
        Vector3 nowDir = transform.forward;
        nowDir = new Vector3(nowDir.x, 0, nowDir.z);
        nowDir = Vector3.Normalize(nowDir);
        float angle = Vector3.Angle(nowDir, originDir);
        Vector3 normal = Vector3.Cross(nowDir, originDir);//叉乘求出法线向量  
        angle *= -Mathf.Sign(Vector3.Dot(normal, Vector3.up));
        _params.rotation = angle;

        _particleSystem.Emit(_params,1);

            Debug.Log(nowDir);
    }

}
