/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  24 March 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include "clipper.svg.h"

namespace Clipper2Lib {

  const char svg_xml_header_0[] =
		  "<?xml version=\"1.0\" standalone=\"no\"?>\n"
          "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n"
          "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n\n <svg width=\"";
  const char svg_xml_header_1[] = "\" height=\"";
  const char svg_xml_header_2[] = "\" viewBox=\"0 0 ";
  const char svg_xml_header_3[] = "\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n\n";


  const char svg_xml_0[] = "\"\n    style=\"fill:";
  const char svg_xml_1[] = "; fill-opacity:";
  const char svg_xml_2[] = "; fill-rule:";
  const char svg_xml_3[] = "; stroke:";
  const char svg_xml_4[] = "; stroke-opacity:";
  const char svg_xml_5[] = "; stroke-width:";
  const char svg_xml_6[] = ";\"/>\n";

  inline std::string ColorToHtml(unsigned clr)
  {
    std::stringstream ss;
    ss << '#' << std::hex << std::setfill('0') << std::setw(6) << (clr & 0xFFFFFF);
    return ss.str();
  }
  //------------------------------------------------------------------------------

  inline float GetAlphaAsFrac(unsigned int clr)
  {
    return ((float)(clr >> 24) / 255.0f);
  }
  //------------------------------------------------------------------------------

  void SvgWriter::Clear()
  {
    for (PathInfoList::iterator pi_iter = path_infos.begin();
      pi_iter != path_infos.end(); ++pi_iter) delete (*pi_iter);
    path_infos.resize(0);
  }
  //------------------------------------------------------------------------------

  void SvgWriter::SetCoordsStyle(const std::string &font_name,
      unsigned font_color, unsigned font_size)
  {
    coords_style.font_name = font_name;
    coords_style.font_color = font_color;
    coords_style.font_size = font_size;
  }
  //------------------------------------------------------------------------------

  void SvgWriter::AddText(const std::string &text,
    unsigned font_color, unsigned font_size, double x, double y)
  {
      text_infos.push_back(new TextInfo(text, "", font_color, 600, font_size, x, y));
  }
  //------------------------------------------------------------------------------

  void SvgWriter::AddPath(const Path64& path, bool is_open, FillRule fillrule,
    unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords)
  {
    int error_code = 0;
    if (path.size() == 0) return;
    PathsD tmp;
    tmp.push_back(ScalePath<double, int64_t>(path, scale_, error_code));
    if (error_code) return;
    PathInfo* pi = new PathInfo(tmp, is_open, fillrule,
      brush_color, pen_color, pen_width, show_coords);
    path_infos.push_back(pi);
  }
  //------------------------------------------------------------------------------

  void SvgWriter::AddPath(const PathD &path, bool is_open, FillRule fillrule,
    unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords)
  {
    if (path.size() == 0) return;
    PathsD p;
    p.push_back(path);
    path_infos.push_back(new PathInfo(p, is_open, fillrule,
      brush_color, pen_color, pen_width, show_coords));
  }
  //------------------------------------------------------------------------------

  void SvgWriter::AddPaths(const Paths64& paths, bool is_open, FillRule fillrule,
    unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords)
  {
    int error_code = 0;
    if (paths.size() == 0) return;
    PathsD tmp = ScalePaths<double, int64_t>(paths, scale_, error_code);
    if (error_code) return;
    PathInfo* pi = new PathInfo(tmp, is_open, fillrule,
      brush_color, pen_color, pen_width, show_coords);
    path_infos.push_back(pi);
  }
  //------------------------------------------------------------------------------

  void SvgWriter::AddPaths(const PathsD &paths, bool is_open, FillRule fillrule,
    unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords)
  {
    if (paths.size() == 0) return;
    path_infos.push_back(new PathInfo(paths, is_open, fillrule,
      brush_color, pen_color, pen_width, show_coords));
  }
  //------------------------------------------------------------------------------

  void SvgWriter::DrawCircle(std::ofstream& file, double x, double y, double radius)
  {
    file << "  <circle cx = \"" << x << "\" cy = \"" << y << "\" r = \"" << radius
      << "\" stroke = \"none\" fill = \"red\" />\n";
  }
  //------------------------------------------------------------------------------

  PathsD SimulateNegativeFill(const PathsD paths)
  {
    return Union(paths, FillRule::Negative);
  }
  //------------------------------------------------------------------------------

  PathsD SimulatePositiveFill(const PathsD paths)
  {
    return Union(paths, FillRule::Positive);
  }
  //------------------------------------------------------------------------------

  bool SvgWriter::SaveToFile(const std::string &filename,
    int max_width, int max_height, int margin)
  {
    RectD rec = InvalidRectD;
    for (const PathInfo* pi : path_infos)
      for (const PathD& path : pi->paths_)
        for (const PointD& pt : path){
          if (pt.x < rec.left) rec.left = pt.x;
          if (pt.x > rec.right) rec.right = pt.x;
          if (pt.y < rec.top) rec.top = pt.y;
          if (pt.y > rec.bottom) rec.bottom = pt.y;
        }
    if (rec.IsEmpty()) return false;

    if (margin < 20) margin = 20;
    if (max_width < 100) max_width = 100;
    if (max_height < 100) max_height = 100;
    double  scale = std::min(
      static_cast<double>(max_width - margin * 2) / rec.Width(),
      static_cast<double>(max_height - margin * 2) / rec.Height());

    rec.Scale(scale);
    double offsetX = margin -rec.left;
    double offsetY = margin -rec.top;

    std::ofstream file;
    file.open(filename);
    if (!file.is_open()) return false;
    file.setf(std::ios::fixed);
    file.precision(0);
    file << svg_xml_header_0 <<
      max_width << "px" << svg_xml_header_1 <<
      max_height << "px" << svg_xml_header_2 <<
      max_width << " " <<
      max_height << svg_xml_header_3;
    setlocale(LC_NUMERIC, "C");
    file.precision(2);

    for (PathInfo* pi : path_infos)
    {
      if (pi->is_open_path || GetAlphaAsFrac(pi->brush_color_) == 0 ||
        (pi->fillrule_ != FillRule::Positive && pi->fillrule_ != FillRule::Negative))
          continue;

      PathsD ppp = pi->fillrule_ == FillRule::Positive ?
        SimulatePositiveFill(pi->paths_) :
        SimulateNegativeFill(pi->paths_);

      file << "  <path d=\"";
      for (PathD& path : ppp)
      {
        if (path.size() < 2 || (path.size() == 2 && !pi->is_open_path)) continue;
        file << " M " << (static_cast<double>(path[0].x) * scale + offsetX) <<
          " " << (static_cast<double>(path[0].y) * scale + offsetY);
        for (PointD& pt : path)
          file << " L " << (pt.x * scale + offsetX) << " "
          << (pt.y * scale + offsetY);
        if (!pi->is_open_path)  file << " z";
      }

      file << svg_xml_0 << ColorToHtml(pi->brush_color_) <<
        svg_xml_1 << GetAlphaAsFrac(pi->brush_color_) <<
        svg_xml_2 << "evenodd" <<
        svg_xml_3 << ColorToHtml(0) <<
        svg_xml_4 << GetAlphaAsFrac(0) <<
        svg_xml_5 << pi->pen_width_ << svg_xml_6;
    }

    for (PathInfo* pi : path_infos)
    {
      unsigned brushColor =
        (pi->fillrule_ == FillRule::Positive || pi->fillrule_ == FillRule::Negative) ?
        0 : pi->brush_color_;

      file << "  <path d=\"";
      for (PathD& path : pi->paths_)
      {
        if (path.size() < 2 || (path.size() == 2 && !pi->is_open_path)) continue;
        file << " M " << (static_cast<double>(path[0].x) * scale + offsetX) <<
          " " << (static_cast<double>(path[0].y) * scale + offsetY);
        for (PointD& pt : path)
          file << " L " << (pt.x * scale + offsetX) << " "
            << (pt.y * scale + offsetY);
        if(!pi->is_open_path)  file << " z";
      }

      file << svg_xml_0 << ColorToHtml(brushColor) <<
        svg_xml_1 << std::setprecision(2) << GetAlphaAsFrac(brushColor) <<
        svg_xml_2 << (pi->fillrule_ == FillRule::NonZero ? "nonzero" : "evenodd") <<
        svg_xml_3 << ColorToHtml(pi->pen_color_) <<
        svg_xml_4 << GetAlphaAsFrac(pi->pen_color_) <<
        svg_xml_5 << pi->pen_width_ << svg_xml_6;

      if (pi->show_coords_) {
        file << std::setprecision(0)  <<
          "  <g font-family=\"" << coords_style.font_name << "\" font-size=\"" <<
          coords_style.font_size  << "\" fill=\""<< ColorToHtml(coords_style.font_color) <<
          "\" fill-opacity=\"" << GetAlphaAsFrac(coords_style.font_color) << "\">\n";
        for (const PathD& path : pi->paths_)
        {
          size_t path_len = path.size();
          if (path_len < 2 || (path_len == 2 && !pi->is_open_path)) continue;
          for (const PointD& pt : path)
            file << "    <text x=\"" << static_cast<int>(pt.x * scale + offsetX) <<
              "\" y=\"" << static_cast<int>(pt.y * scale + offsetY) << "\">" <<
              pt.x << "," << pt.y << "</text>\n";
        }
        file << "  </g>\n\n";
      }
    }

    ////draw red dots at all solution vertices - useful for debugging
    //for (PathInfo* pi : path_infos)
    //  if (!(pi->pen_color_ & 0x00FF00FF)) // ie any shade of green only
    //    for (PathD& path : pi->paths_)
    //      for (PointD& pt : path)
    //        DrawCircle(file, pt.x * scale + offsetX, pt.y * scale + offsetY, 1.6);

    for (TextInfo* ti : text_infos)
    {
      file << "  <g font-family=\"" << ti->font_name << "\" font-size=\"" <<
        ti->font_size << "\" fill=\"" << ColorToHtml(ti->font_color) <<
        "\" fill-opacity=\"" << GetAlphaAsFrac(ti->font_color) << "\">\n";
      file << "    <text x=\"" << static_cast<int>(ti->x * scale + offsetX) << 
        "\" y=\"" << static_cast<int>(ti->y * scale + offsetY) << "\">" <<
        ti->text << "</text>\n  </g>\n\n";
    }

    file << "</svg>\n";
    file.close();
    setlocale(LC_NUMERIC, "");
    return true;
  }

  //------------------------------------------------------------------------------
  //------------------------------------------------------------------------------

  bool SkipBlanks(std::string::const_iterator& si,
    const std::string::const_iterator se)
  {
    while (si != se && *si <= ' ') ++si;
    return si != se;
  }
  //------------------------------------------------------------------------------

  bool GetNum(std::string::const_iterator& si,
    const std::string::const_iterator se, double& value)
  {
    while (si != se && *si <= ' ') ++si;
    if (si != se &&  *si == ',') ++si;
    while (si != se && *si <= ' ') ++si;
    if (si == se) return false;
    std::string::const_iterator sit = si;
    while ((si != se && (*si <= ' ')) || *si == ',') ++si;
    bool isneg = *si == '-';
    if (isneg) ++si;
    value = 0;
    int loop_cnt = 0, decpos = -1;
    while (si != se)
    {
      if (*si == '.')
      {
        if (loop_cnt == 0 || decpos >= 0) return false; //invalid
        else decpos = loop_cnt;
      }
      else if (*si < '0' || *si > '9') break;
      else value = value * 10 + (long)(*si) - (long)'0';
      ++si; loop_cnt++;
    }
    if (decpos >= 0)
    {
      decpos = loop_cnt - decpos - 1;
      value *= pow(10, -decpos);
    }
    if (isneg) value = -value;
    return si != sit;
  }
  //------------------------------------------------------------------------------

  bool GetCommand(std::string::const_iterator& si,
    char& command, bool& is_relative)
  {
    if (*si >= 'a' && *si <= 'z')
    {
      is_relative = true;
      command = toupper(*si);
    }
    else if (*si >= 'A' && *si <= 'Z')
    {
      command = *si;
      is_relative = false;
    }
    else return false; //ie leave command and is_relative unchanged!
    ++si; //only increment the offset with a valid command
    return true;
  }
  //------------------------------------------------------------------------------

  inline bool Find(const std::string& text,
    std::string::const_iterator& start, const std::string::const_iterator& end)
  {
    start = std::search(start, end, text.cbegin(), text.cend());
    return start != end;
  }
  //------------------------------------------------------------------------------

  bool SvgReader::LoadPath(std::string::const_iterator& p,
    const std::string::const_iterator& pe)
  {
    if (!Find("d=\"", p, pe)) return false;
    p += 3;
    if (!SkipBlanks(p, pe)) return false;
    char command;
    bool is_relative;
    int vals_needed = 2;
    //nb: M == absolute move, m == relative move
    if (!GetCommand(p, command, is_relative) || command != 'M') return false;
    double vals[2] { 0, 0 };
    double x = 0, y = 0;
    ++p;
    if (!GetNum(p, pe, x) || !GetNum(p, pe, y)) return false;
    PathsD ppp;
    PathD pp;
    pp.push_back(PointD(x, y));
    while (SkipBlanks(p, pe))
    {
      if (GetCommand(p, command, is_relative))
      {
        switch (command) {
        case 'L':
        case 'M': {vals_needed = 2;  break; }
        case 'H':
        case 'V': {vals_needed = 1;  break; }
        case 'Z': {
            if (pp.size() > 2) ppp.push_back(pp);
            pp.clear();
            vals_needed = 0;
            break;
        }
        default: vals_needed = -1;
        }
        if (vals_needed < 0) break; //oops!
      }
      else
      {
        for (int i = 0; i < vals_needed; ++i)
            if (!GetNum(p, pe, vals[i])) vals_needed = -1;
        if (vals_needed <= 0) break; //oops!
        switch (vals_needed) {
          case 1:
          {
              if (command == 'V') y = (is_relative ? y + vals[0] : vals[0]);
              else x = (is_relative ? x + vals[0] : vals[0]);
              break;
          }
          case 2:
          {
              x = (is_relative ? x + vals[0] : vals[0]);
              y = (is_relative ? y + vals[1] : vals[1]);
              break;
          }
          default: break;
        }
        pp.push_back(PointD(x, y));
      }
    }
    if (pp.size() > 3) ppp.push_back(pp);
    //todo - fix fillrule
    path_infos.push_back(new PathInfo(ppp, false, FillRule::EvenOdd, 0, 0xFF000000, 1, false));
    return  (ppp.size() > 0);
  }

  bool SvgReader::LoadFromFile(const std::string &filename)
  {
      Clear();
      std::ifstream file(filename);
      if (!file.good()) return false;

      std::stringstream xml_buff;
      xml_buff << file.rdbuf();
      file.close();
      xml = xml_buff.str();
      std::string::const_iterator p = xml.cbegin(), q, xml_end = xml.cend();

      while (Find("<path", p, xml_end))
      {
        p += 6;
        q = p;
        if (!Find("/>", p, xml_end)) break;
        LoadPath(q, p);
        p += 2;
      }
      return path_infos.size() > 0;
  }

  PathsD SvgReader::GetPaths()
  {
    PathsD result;
      for (size_t i = 0; i < path_infos.size(); ++i)
          for (size_t j = 0; j < path_infos[i]->paths_.size(); ++j)
              result.push_back(path_infos[i]->paths_[j]);
      return result;
  }

} //namespace
