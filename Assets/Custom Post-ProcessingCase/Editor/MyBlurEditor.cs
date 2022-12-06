using UnityEditor.Rendering;
using UnityEngine;
using UnityEngine.Rendering.Universal;

[VolumeComponentEditor(typeof(MyBlur))]
public class MyBlurEditor : VolumeComponentEditor
{
    private SerializedDataParameter m_BlurType;
    private SerializedDataParameter m_FilterMode;
    private SerializedDataParameter m_DownSample;

    private SerializedDataParameter m_Iterations;
    private SerializedDataParameter m_BlurSpread;
    private SerializedDataParameter m_BlurRadius;

    private SerializedDataParameter m_Area;
    private SerializedDataParameter m_Spread;
    private SerializedDataParameter m_Offset;
    
    private SerializedDataParameter m_radialCenter;
    private SerializedDataParameter m_angle;
    
    public override void OnEnable()
    {
       var o = new PropertyFetcher<MyBlur>(serializedObject);

        m_BlurType = Unpack(o.Find(x => x.blurType));
        m_FilterMode = Unpack(o.Find(x => x.filterMode));
        m_DownSample = Unpack(o.Find(x => x.downSample));
        m_Iterations = Unpack(o.Find(x => x.iterations));
        m_BlurSpread = Unpack(o.Find(x => x.blurSpread));
        m_BlurRadius = Unpack(o.Find(x => x.blurRadius));
        m_Area = Unpack(o.Find(x => x.area)); 
        m_Spread = Unpack(o.Find(x => x.spread));
        m_Offset = Unpack(o.Find(x => x.offset));
        m_radialCenter = Unpack(o.Find(x => x.radialCenter));
        m_angle = Unpack(o.Find(x => x.angle));
    }

    public override void OnInspectorGUI()
    {
        //base.OnInspectorGUI();
        
        GUILayout.Space(5.0f);
        PropertyField(m_BlurType, new GUIContent("模糊类型"));
        GUILayout.Space(10.0f);
        PropertyField(m_FilterMode, new GUIContent("过滤类型"));
        PropertyField(m_DownSample, new GUIContent("降采样"));
        PropertyField(m_Iterations, new GUIContent("Iterations"));

        switch (m_BlurType.value.intValue)
        {
            case (int)BlurType.GaussianBlur:
                Gaussian();
                break;
            case (int)BlurType.DualBlur:
                DualBlur();
                break;
            case (int)BlurType.BoxBlur:
                BoxBlur();
                break;
            case (int)BlurType.KawaseBlur:
                KawaseBlur();
                break;
            case (int)BlurType.BokehBlur:
                BokehBlur();
                break;
            case (int)BlurType.TileShiftBlur:
                TileShiftBlur();
                break;
            case (int)BlurType.IrisBlur:
                IrisBlur();
                break;
            case (int)BlurType.GrainyBlur:
                GrainyBlur();
                break;
            case (int)BlurType.RadialBlur:
                RadialBlur();
                break;
            case (int)BlurType.DirectionalBlur:
                DirectionalBlur();
                break;
        }
    }

    private void IrisBlur()
    {
        PropertyField(m_BlurRadius, new GUIContent("BlurRadius"));
        PropertyField(m_Area, new GUIContent("Area"));
        PropertyField(m_Spread, new GUIContent("Spread"));
    }

    private void GrainyBlur()
    {
        PropertyField(m_BlurRadius, new GUIContent("BlurRadius"));
    }

    private void RadialBlur()
    {
        PropertyField(m_BlurRadius, new GUIContent("BlurRadius"));
        PropertyField(m_radialCenter, new GUIContent("RadialCenter"));
    }

    private void DirectionalBlur()
    {
        PropertyField(m_BlurRadius, new GUIContent("BlurRadius"));
        PropertyField(m_angle, new GUIContent("Angle"));
    }

    private void TileShiftBlur()
    {
        PropertyField(m_BlurRadius, new GUIContent("BlurRadius"));
        PropertyField(m_Area, new GUIContent("Area"));
        PropertyField(m_Spread, new GUIContent("Spread"));
        PropertyField(m_Offset, new GUIContent("OffsetY"));
    }

    private void BokehBlur()
    {
        PropertyField(m_BlurRadius, new GUIContent("BlurRadius"));
    }

    private void KawaseBlur()
    {
        PropertyField(m_BlurRadius, new GUIContent("BlurRadius"));
    }

    private void BoxBlur()
    {
        PropertyField(m_BlurSpread, new GUIContent("BlurSpread"));
    }

    private void DualBlur()
    {
        PropertyField(m_BlurRadius, new GUIContent("BlurRadius"));
    }

    private void Gaussian()
    {
        PropertyField(m_BlurSpread, new GUIContent("BlurSpread"));
    }
    
}
