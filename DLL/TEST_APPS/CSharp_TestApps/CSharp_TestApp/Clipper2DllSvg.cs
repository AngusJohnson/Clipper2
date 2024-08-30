using System.Diagnostics;
using System.Globalization;
using static Clipper2Dll.Clipper2DllCore;

using RectD = Clipper2Dll.Clipper2DllCore.Rect<double>;

namespace Clipper2Dll
{
  public class SvgWriter
  {
    private static RectD rectMax = InvalidRectD;

    public static RectD RectEmpty = new RectD(0, 0, 0, 0);
    internal static bool IsValidRect(RectD rec)
    {
      return rec.right >= rec.left && rec.bottom >= rec.top;
    }

    private readonly struct CoordStyle
    {
      public readonly string FontName;
      public readonly int FontSize;
      public readonly uint FontColor;
      public CoordStyle(string fontname, int fontsize, uint fontcolor)
      {
        FontName = fontname;
        FontSize = fontsize;
        FontColor = fontcolor;
      }
    }
    private readonly struct TextInfo
    {
      public readonly string text;
      public readonly int fontSize;
      public readonly uint fontColor;
      public readonly double posX;
      public readonly double posY;
      public TextInfo(string text, double x, double y,
        int fontsize = 12, uint fontcolor = 0xFF000000)
      {
        this.text = text;
        posX = x;
        posY = y;
        fontSize = fontsize;
        fontColor = fontcolor;
      }
    }

    private readonly struct CircleInfo
    {
      public readonly double  centerX;
      public readonly double  centerY;
      public readonly double  radius;
      public readonly uint    brushClr;
      public readonly uint    penClr;
      public readonly uint    penWidth;
      public CircleInfo(double centerX, double centerY,
        double radius, uint brushClr, uint penClr, uint penWidth)
      {
        this.centerX = centerX;
        this.centerY = centerY;
        this.radius = radius;
        this.brushClr = brushClr;
        this.penClr = penClr;
        this.penWidth = penWidth;
      }
    }

    private readonly struct PolyInfo
    {
      public readonly double[] paths;
      public readonly uint BrushClr;
      public readonly uint PenClr;
      public readonly double PenWidth;
      public readonly bool ShowCoords;
      public readonly bool IsOpen;
      public PolyInfo(double[] paths, uint brushcolor, uint pencolor,
        double penwidth, bool showcoords = false, bool isopen = false)
      {
        this.paths = new double[paths.Length];
        paths.CopyTo(this.paths, 0);
        BrushClr = brushcolor;
        PenClr = pencolor;
        PenWidth = penwidth;
        ShowCoords = showcoords;
        IsOpen = isopen;
      }
    }

    public FillRule FillRule { get; set; }
    private readonly List<PolyInfo> PolyInfoList = new List<PolyInfo>();
    private readonly List<TextInfo> textInfos = new List<TextInfo>();
    private readonly List<CircleInfo> circleInfos = new List<CircleInfo>();
    private readonly CoordStyle coordStyle;

    private const string svg_header = "<?xml version=\"1.0\" standalone=\"no\"?>\n" +
      "<svg width=\"{0}px\" height=\"{1}px\" viewBox=\"0 0 {0} {1}\"" +
      " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n\n";
    private const string svg_path_format = "\"\n style=\"fill:{0};" +
        " fill-opacity:{1:f2}; fill-rule:{2}; stroke:{3};" +
        " stroke-opacity:{4:f2}; stroke-width:{5:f2};\"/>\n\n";
    private const string svg_path_format2 = "\"\n style=\"fill:none; stroke:{0};" +
        "stroke-opacity:{1:f2}; stroke-width:{2:f2};\"/>\n\n";

    public SvgWriter(FillRule fillrule = FillRule.EvenOdd,
      string coordFontName = "Verdana", int coordFontsize = 9, uint coordFontColor = 0xFF000000)
    {
      coordStyle = new CoordStyle(coordFontName, coordFontsize, coordFontColor);
      FillRule = fillrule;
    }

    public void ClearPaths()
    {
      PolyInfoList.Clear();
    }

    public void ClearText()
    {
      textInfos.Clear();
    }

    public void ClearCircles()
    {
      circleInfos.Clear();
    }

    public void ClearAll()
    {
      PolyInfoList.Clear();
      textInfos.Clear();
      circleInfos.Clear();
    }
    public void AddClosedPath(long[] path, uint brushColor,
      uint penColor, double penWidth, bool showCoords = false)
    {
      long[] tmp = new long[path.Count() +2]; 
      path.CopyTo(tmp, 2);
      tmp[0] = tmp.Length;
      tmp[1] = 1;
      AddClosedPaths(tmp, brushColor, penColor, penWidth, showCoords);
    }

    public void AddClosedPath(double[] path, uint brushColor,
      uint penColor, double penWidth, bool showCoords = false)
    {
      double[] tmp = new double[path.Length + 2];
      path.CopyTo(tmp, 2);
      tmp[0] = tmp.Length;
      tmp[1] = 1;
      AddClosedPaths(tmp, brushColor, penColor, penWidth, showCoords);
    }

    public void AddClosedPaths(long[] paths, uint brushColor,
      uint penColor, double penWidth, bool showCoords = false)
    {
      if (paths.Length == 0) return;
      double[] pathsD;
      ConvertArrayOfLongs(paths, out pathsD);
      PolyInfoList.Add(new PolyInfo(pathsD, brushColor, penColor, penWidth, showCoords, false));
    }

    public void AddClosedPaths(double[] paths, uint brushColor,
      uint penColor, double penWidth, bool showCoords = false)
    {
      if (paths.Length == 0) return;
      PolyInfoList.Add(new PolyInfo(paths, brushColor, penColor, penWidth, showCoords, false));
    }

    public void AddOpenPath(long[] path, uint penColor,
      double penWidth, bool showCoords = false)
    {
      long[] tmp = new long[path.Length + 2];
      path.CopyTo(tmp, 2);
      tmp[0] = tmp.Length;
      tmp[1] = 1;
      AddOpenPaths(tmp, penColor, penWidth, showCoords);
    }

    public void AddOpenPath(double[] path, uint penColor,
      double penWidth, bool showCoords = false)
    {
      double[] tmp = new double[path.Length + 2];
      path.CopyTo(tmp, 2);
      tmp[0] = tmp.Length;
      tmp[1] = 1;
      AddOpenPaths(tmp, penColor, penWidth, showCoords);
    }

    public void AddOpenPaths(long[] paths,
      uint penColor, double penWidth, bool showCoords = false)
    {
      if (paths.Length == 0) return;
      double[] pathsD;
      ConvertArrayOfLongs(paths, out pathsD);
      PolyInfoList.Add(new PolyInfo(pathsD, 0x0, penColor, penWidth, showCoords, true));
    }

    public void AddOpenPaths(double[] paths, uint penColor,
      double penWidth, bool showCoords = false)
    {
      if (paths.Length == 0) return;
      PolyInfoList.Add(new PolyInfo(paths,
        0x0, penColor, penWidth, showCoords, true));
    }


    public void AddText(string cap, double posX, double posY, int fontSize, uint fontClr = 0xFF000000)
    {
      textInfos.Add(new TextInfo(cap, posX, posY, fontSize, fontClr));
    }

    public void AddCircle(double centerX, double centerY,
        double radius, uint brushClr, uint penClr, uint penWidth)
    {
      circleInfos.Add(new CircleInfo(centerX, centerY, radius, brushClr, penClr, penWidth));
    }

    private RectD GetBounds()
    {
      RectD bounds = new RectD(double.MaxValue, double.MaxValue, -double.MaxValue, -double.MaxValue);
      foreach (PolyInfo pi in PolyInfoList)
      {
        int idx = 2;
        long pathsCnt = Convert.ToInt64(pi.paths[1]);
        for (int i = 0; i < pathsCnt; i++)
        {
          long pathLen = Convert.ToInt64(pi.paths[idx]);
          idx += 2;
          for (int j = 0; j < pathLen; j++)
          {
            double x = pi.paths[idx++];
            double y = pi.paths[idx++];
#if USINGZ
            idx++;
#endif
            if (x < bounds.left) bounds.left = x;
            if (x > bounds.right) bounds.right = x;
            if (y < bounds.top) bounds.top = y;
            if (y > bounds.bottom) bounds.bottom = y;
          }

        }
      }
      if (!IsValidRect(bounds)) return RectEmpty;
      return bounds;
    }

    private static string ColorToHtml(uint clr)
    {
      return '#' + (clr & 0xFFFFFF).ToString("X6");
    }

    private static float GetAlpha(uint clr)
    {
      return ((float)(clr >> 24) / 255);
    }

    public bool SaveToFile(string filename, int maxWidth = 0, int maxHeight = 0, int margin = -1)
    {
      if (margin < 0) margin = 20;
      RectD bounds = GetBounds();
      if (bounds.IsEmpty()) return false;

      double scale = 1.0;
      if (maxWidth > 0 && maxHeight > 0)
        scale = Math.Min(
           (maxWidth - margin * 2) / bounds.Width(),
            (maxHeight - margin * 2) / bounds.Height());

      long offsetX = margin - (long)(bounds.left * scale);
      long offsetY = margin - (long)(bounds.top * scale);

      StreamWriter writer;
      try
      {
        writer = new StreamWriter(filename);
      }
      catch
      {
        return false;
      }

      if (maxWidth <= 0 || maxHeight <= 0)
        writer.Write(svg_header, (bounds.right - bounds.left) + margin * 2,
          (bounds.bottom - bounds.top) + margin * 2);
      else
        writer.Write(svg_header, maxWidth, maxHeight);

      foreach (PolyInfo pi in PolyInfoList)
      {
        writer.Write(" <path d=\"");

        long idx = 2;
        long pathsCnt = Convert.ToInt64(pi.paths[1]);
        for (int i = 0; i < pathsCnt; i++)
        {
          long pathLen = Convert.ToInt64(pi.paths[idx]);
          idx += 2;

          if (!pi.IsOpen && pathLen < 3) { 
            idx += pathLen * VERTEX_FIELD_CNT;
            continue;
          }

          writer.Write(string.Format(NumberFormatInfo.InvariantInfo, " M {0:f2} {1:f2}",
            (pi.paths[idx++] * scale + offsetX), (pi.paths[idx++] * scale + offsetY)));
#if USINGZ
          idx++;
#endif
          for (int j = 1; j < pathLen; j++)
          {
            writer.Write(string.Format(NumberFormatInfo.InvariantInfo, " L {0:f2} {1:f2}",
            (pi.paths[idx++] * scale + offsetX), (pi.paths[idx++] * scale + offsetY)));
#if USINGZ
            idx++;
#endif
          }
          if (!pi.IsOpen) writer.Write(" z");
        }

        if (!pi.IsOpen)
          writer.Write(string.Format(NumberFormatInfo.InvariantInfo, svg_path_format,
              ColorToHtml(pi.BrushClr), GetAlpha(pi.BrushClr),
              (FillRule == FillRule.EvenOdd ? "evenodd" : "nonzero"),
              ColorToHtml(pi.PenClr), GetAlpha(pi.PenClr), pi.PenWidth));
        else
          writer.Write(string.Format(NumberFormatInfo.InvariantInfo, svg_path_format2,
              ColorToHtml(pi.PenClr), GetAlpha(pi.PenClr), pi.PenWidth));

        if (pi.ShowCoords)
        {
          writer.Write("<g font-family=\"{0}\" font-size=\"{1}\" fill=\"{2}\">\n",
            coordStyle.FontName, coordStyle.FontSize, ColorToHtml(coordStyle.FontColor));

            idx = 2;
            pathsCnt = Convert.ToInt64(pi.paths[1]);
            for (int i = 0; i < pathsCnt; i++)
            {
              long pathLen = Convert.ToInt64(pi.paths[idx]);
              idx += 2;
              for (int j = 1; j < pathLen; j++)
              {
              double x = pi.paths[idx++];
              double y = pi.paths[idx++];
#if USINGZ
              double z = pi.paths[idx++];
              writer.Write("<text x=\"{0:f2}\" y=\"{1:f2}\">{2:f2},{3:f2},{4}</text>\n",
                (x * scale + offsetX), (y * scale + offsetY), x, y, z);
#else
              writer.Write("<text x=\"{0:f2}\" y=\"{1:f2}\">{2:f2},{3:f2}</text>\n", 
                (x * scale + offsetX), (y * scale + offsetY), x, y);
#endif
            }
          }
          writer.Write("</g>\n\n");
        }
      }

      foreach (CircleInfo circInfo in circleInfos)
      {
        writer.Write("<g>\n  <circle cx=\"{0:f2}\" cy=\"{1:f2}\"  r=\"{2:f2}\"" +
          " stroke=\"{3}\" stroke-width=\"{4}\" fill=\"{5}\" opacity=\"{6:f2}\" />\n</g>\n",
                     circInfo.centerX * scale + offsetX, circInfo.centerY * scale + offsetY, 
                     circInfo.radius * scale, ColorToHtml(circInfo.penClr), circInfo.penWidth,
                     ColorToHtml(circInfo.brushClr), GetAlpha(circInfo.brushClr));
      }


      foreach (TextInfo captionInfo in textInfos)
      {
        writer.Write("<g font-family=\"Verdana\" font-style=\"normal\" " +
                     "font-weight=\"normal\" font-size=\"{0}\" fill=\"{1}\">\n",
                     captionInfo.fontSize, ColorToHtml(captionInfo.fontColor));
        writer.Write("<text x=\"{0:f2}\" y=\"{1:f2}\">{2}</text>\n</g>\n",
          captionInfo.posX * scale + offsetX, captionInfo.posY * scale + offsetY, captionInfo.text);
      }

      writer.Write("</svg>\n");
      writer.Close();
      return true;
    }
  } //end SvgWriter

public static class SvgWriterUtils
  {
    public static void AddCaption(SvgWriter svg, string caption, int x, int y)
    {
      svg.AddText(caption, x, y, 14);
    }

    public static void AddSubject(SvgWriter svg, long[] path)
    {
      svg.AddClosedPath(path, 0x1800009C, 0xAAB3B3DA, 0.8);
    }
    public static void AddSubject(SvgWriter svg, double[] path)
    {
      svg.AddClosedPath(path, 0x1800009C, 0xAAB3B3DA, 0.8);
    }

    public static void AddSubjects(SvgWriter svg, long[] paths)
    {
      svg.AddClosedPaths(paths, 0x1800009C, 0xAAB3B3DA, 0.8);
    }
    public static void AddOpenSubjects(SvgWriter svg, long[] paths)
    {
      svg.AddOpenPaths(paths, 0xAAB3B3DA, 0.8);
    }

    public static void AddSubjects(SvgWriter svg, double[] paths)
    {
      svg.AddClosedPaths(paths, 0x1800009C, 0xAAB3B3DA, 0.8);
    }

    public static void AddOpenSubjects(SvgWriter svg, double[] paths)
    {
      svg.AddOpenPaths(paths, 0xAAB3B3DA, 1.2);
    }

    public static void AddClip(SvgWriter svg, long[] path)
    {
      svg.AddClosedPath(path, 0x129C0000, 0xCCFFA07A, 0.8);
    }

    public static void AddClip(SvgWriter svg, double[] path)
    {
      svg.AddClosedPath(path, 0x129C0000, 0xCCFFA07A, 0.8);
    }

    public static void AddClips(SvgWriter svg, long[] paths)
    {
      svg.AddClosedPaths(paths, 0x129C0000, 0xCCFFA07A, 0.8);
    }

    public static void AddClips(SvgWriter svg, double[] paths)
    {
      svg.AddClosedPaths(paths, 0x129C0000, 0xCCFFA07A, 0.8);
    }

    public static void AddSolution(SvgWriter svg, long[] paths,
      bool show_coords, bool is_closed = true, bool is_joined = true)
    {
      svg.AddClosedPaths(paths, 0x4080ff9C, 0xFF003300, 1.5, show_coords);
    }

    public static void AddOpenSolution(SvgWriter svg, long[] paths, bool show_coords)
    {
      svg.AddOpenPaths(paths, 0xFF003300, 2.2, show_coords);
    }

    public static void AddSolution(SvgWriter svg, double[] paths, bool show_coords)
    {
      svg.AddClosedPaths(paths, 0x4080ff9C, 0xFF003300, 1.5, show_coords);
    }

    public static void AddOpenSolution(SvgWriter svg, double[] paths, bool show_coords)
    {
      svg.AddOpenPaths(paths, 0xFF003300, 2.2, show_coords);
    }

    public static void OpenFileWithDefaultApp(string filename)
    {
      string path = Path.GetFullPath(filename);
      if (!File.Exists(path)) return;
      Process p = new Process() { StartInfo = new ProcessStartInfo(path) { UseShellExecute = true } };
      p.Start();
    }

  } //end SvgWriterUtils
}
