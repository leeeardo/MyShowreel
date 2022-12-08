using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

[ExecuteInEditMode]
public class SnowTraceSettings : MonoBehaviour
{
    public Shader _drawTraceShader;

    public Transform[] traceTarget;

    public float brushSize;
    public float brushStrength;
    
    public Shader _snowShader;
    private Material _drawtraceMaterial;
    private int _layerMask;
    private RaycastHit _hit;

    private RenderTexture _traceRT;

    // Start is called before the first frame update
    void Start()
    {
        _layerMask = LayerMask.GetMask("TransparentFX");
        //_snowShader = GetComponent<MeshRenderer>().material;
        if (_drawTraceShader != null)
        {
            _drawtraceMaterial = new Material(_drawTraceShader);
        }

        _traceRT = new CustomRenderTexture(1024, 1024, RenderTextureFormat.ARGBFloat);
        Shader.SetGlobalTexture("_SnowTraceMask",_traceRT);
    }

    // Update is called once per frame
    void Update()
    {
        for (int i = 0; i < traceTarget.Length; i++)
        {
            if (Physics.Raycast(traceTarget[i].position,-Vector3.up,out _hit)
                &&traceTarget[i].gameObject.layer==_layerMask)
            {
                _drawtraceMaterial.SetVector("_TraceCoord",new Vector4(_hit.textureCoord.x,_hit.textureCoord.y,0,0));
                _drawtraceMaterial.SetFloat("_BrushStrength" , brushStrength);
                _drawtraceMaterial.SetFloat("_BrushSize" , brushSize);
                //_hit.point;

            }
        }
    }
}
