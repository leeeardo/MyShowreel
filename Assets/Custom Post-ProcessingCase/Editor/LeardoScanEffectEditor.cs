using UnityEditor;
using UnityEditor.Rendering;
using UnityEngine;
using UnityEngine.Rendering.Universal;

[VolumeComponentEditor(typeof(Scan))]
public class LearoScanEffectEditor : VolumeComponentEditor
{
    //time
    private SerializedDataParameter m_effectTimeSpan;
    private SerializedDataParameter m_fadeOutTime;
    private SerializedDataParameter m_scanSpeed;
    //scanline
    private SerializedDataParameter m_scanWidth;
    private SerializedDataParameter m_scanLineSmoothness;
    private SerializedDataParameter m_scanLineColor;
    
    //scan detail
    private SerializedDataParameter m_dotDensity;
    private SerializedDataParameter m_dotSize;
    private SerializedDataParameter m_isoHeightDensity;
    private SerializedDataParameter m_isoHeightThickness;
    private SerializedDataParameter m_outLine;
    private SerializedDataParameter m_outlineThickness;
    private SerializedDataParameter m_glitch;
    private SerializedDataParameter m_glitchStrength;
    
    //inside color
    private SerializedDataParameter m_decolorationStrength;
    private SerializedDataParameter m_insideBrightness;
    
    private SerializedDataParameter m_fresnel;
    private SerializedDataParameter m_dotTexture;
    
    public override void OnEnable()
    {
       var o = new PropertyFetcher<Scan>(serializedObject);

       m_effectTimeSpan = Unpack(o.Find(x => x.effectTimeSpan));
       m_fadeOutTime = Unpack(o.Find(x => x.fadeOutTime));
       m_scanSpeed = Unpack(o.Find(x => x.scanSpeed));
       
       m_scanWidth = Unpack(o.Find(x => x.scanWidth));
       m_scanLineSmoothness = Unpack(o.Find(x => x.scanLineSmoothness));
       m_scanLineColor = Unpack(o.Find(x => x.scanLineColor));
       
       m_dotDensity = Unpack(o.Find(x => x.dotDensity));
       m_dotSize = Unpack(o.Find(x => x.dotSize));
       m_isoHeightDensity = Unpack(o.Find(x => x.isoHeightDensity));
       m_isoHeightThickness = Unpack(o.Find(x => x.isoHeightThickness));
       m_outLine = Unpack(o.Find(x => x.outLine));
       m_outlineThickness = Unpack(o.Find(x => x.outlineThickness));
       m_glitch = Unpack(o.Find(x => x.glitch));
       m_glitchStrength = Unpack(o.Find(x => x.glitchStrength));
       
       m_decolorationStrength = Unpack(o.Find(x => x.decolorationStrength));
       m_insideBrightness = Unpack(o.Find(x => x.insideBrightness));
       
       m_fresnel = Unpack(o.Find(x => x.fresnel));
       m_dotTexture = Unpack(o.Find(x => x.dotTexture));
    }

    public override void OnInspectorGUI()
    {
        GUIStyle headStyle = new GUIStyle();
        headStyle.fontSize = 13;
        headStyle.normal.textColor = new Color(0.9f,0.9f,0.9f,1);
        headStyle.alignment = TextAnchor.MiddleLeft;
        
        GUILayout.Label("扫描时间相关设置",headStyle);
        PropertyField(m_effectTimeSpan,new GUIContent("扫描持续时间"));
        PropertyField(m_fadeOutTime,new GUIContent("消失开始时间"));
        PropertyField(m_scanSpeed,new GUIContent("扫描速度"));
        
        GUILayout.Label("扫描线设置",headStyle);
        PropertyField(m_scanWidth,new GUIContent("扫描最大范围"));
        PropertyField(m_scanLineSmoothness,new GUIContent("扫描线宽度"));
        PropertyField(m_scanLineColor,new GUIContent("扫描线颜色"));
        
        GUILayout.Label("细节设置",headStyle);
        PropertyField(m_dotDensity,new GUIContent("扫描点密度"));
        PropertyField(m_dotSize,new GUIContent("扫描点大小"));
        PropertyField(m_isoHeightDensity,new GUIContent("等高线密度"));
        PropertyField(m_isoHeightThickness,new GUIContent("等高线厚度"));
        PropertyField(m_outLine,new GUIContent("描边开关"));
        PropertyField(m_outlineThickness,new GUIContent("描边厚度"));
        PropertyField(m_glitch,new GUIContent("Glitch开关"));
        PropertyField(m_glitchStrength,new GUIContent("Glitch偏移强度"));
        
        GUILayout.Label("扫描线内部设置",headStyle);
        PropertyField(m_decolorationStrength,new GUIContent("内部去色强度"));
        PropertyField(m_insideBrightness,new GUIContent("内部亮度"));
        PropertyField(m_fresnel,new GUIContent("fresnel开关"));
        PropertyField(m_dotTexture,new GUIContent("扫描点贴图"));
        
        
    }
}
