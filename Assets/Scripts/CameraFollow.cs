using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CameraFollow : MonoBehaviour
{
    public GameObject target;

    private Vector3 position;

    private Vector3 distance;
    // Start is called before the first frame update
    void Start()
    {
        position = target.transform.position;
        position.y = 0;
        distance = transform.position - position;
    }

    // Update is called once per frame
    void Update()
    {
        //transform.LookAt(target.transform);
        position = target.transform.position;
        position.y = 0;
        transform.position = position + distance;
    }
}
