using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;

public class GetMouseButtomWorldPos : MonoBehaviour
{
    private Shader _shader;
    private float scanTimer = 0;

    private Camera _camera;

    public GameObject volumeObject;
    private Volume myVolume;

    private Scan scan;

    private bool isScaning;
    // Start is called before the first frame update
    void Start()
    {
        _shader = Shader.Find("Custom/Postprocess/Scan");
        _camera = Camera.main;
        myVolume = volumeObject.GetComponent<Volume>();
        myVolume.profile.TryGet(typeof(Scan), out scan);
        isScaning = false;
    }

    // Update is called once per frame
    void Update()
    {
        if (Input.GetMouseButtonDown(0))
        {
            Ray ray = _camera.ScreenPointToRay(Input.mousePosition);
            RaycastHit hit;
            bool isCollider = Physics.Raycast(ray, out hit);


            Vector3 hitPos = hit.point;
            Shader.SetGlobalVector("_HitPos",hitPos);
        }
        if (Input.GetMouseButtonDown(0))
        {
            scanTimer = 0;
            isScaning = true;
        }

        scanTimer += Time.deltaTime;
        
        if (scanTimer<scan.effectTimeSpan.value&& isScaning)
        {
            Shader.SetGlobalFloat("_ScanTimer" , scanTimer);
        }
        else
        {
            isScaning = false;
            Shader.SetGlobalFloat("_ScanTimer" , 0);

        }
        
    }

        
        
}
