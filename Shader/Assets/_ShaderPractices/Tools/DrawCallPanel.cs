using System.Text;
using Unity.Profiling;
using UnityEngine;

public class DrawCallPanel : MonoBehaviour
{
#if UNITY_EDITOR
    [SerializeField, Range(1, 50)]
    private float size;
    private string m_statsText;
    private ProfilerRecorder m_setPassCallsRecorder;
    private ProfilerRecorder m_drawCallsRecorder;
    private ProfilerRecorder m_trianglesRecorder;
    private ProfilerRecorder m_verticesRecorder;

    private void OnEnable()
    {
        m_setPassCallsRecorder = ProfilerRecorder.StartNew(ProfilerCategory.Render, "SetPass Calls Count");
        m_drawCallsRecorder = ProfilerRecorder.StartNew(ProfilerCategory.Render, "Draw Calls Count");
        m_trianglesRecorder = ProfilerRecorder.StartNew(ProfilerCategory.Render, "Triangles Count");
        m_verticesRecorder = ProfilerRecorder.StartNew(ProfilerCategory.Render, "Vertices Count");
    }

    private void OnDisable()
    {
        m_setPassCallsRecorder.Dispose();
        m_drawCallsRecorder.Dispose();
        m_trianglesRecorder.Dispose();
        m_verticesRecorder.Dispose();
    }

    private void Update()
    {
        var sb = new StringBuilder(500);
        if (m_setPassCallsRecorder.Valid) sb.AppendLine($"SetPass Calls: {m_setPassCallsRecorder.LastValue}");
        if (m_drawCallsRecorder.Valid) sb.AppendLine($"Draw Calls: {m_drawCallsRecorder.LastValue}");
        if (m_trianglesRecorder.Valid) sb.AppendLine($"Triangles: {m_trianglesRecorder.LastValue}");
        if (m_verticesRecorder.Valid) sb.AppendLine($"Vertices: {m_verticesRecorder.LastValue}");
        m_statsText = sb.ToString();
    }

    private void OnGUI()
    {
        GUIStyle uIStyle = new GUIStyle();
        uIStyle.fontSize = (int)size * 15;
        uIStyle.fontStyle = FontStyle.Bold;
        uIStyle.normal.textColor = Color.white;
        GUI.TextArea(new Rect(10, 30, 250 * size, 65 * size), m_statsText, uIStyle);
    }

#endif
}