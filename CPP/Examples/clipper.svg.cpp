/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  21 November 2020                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2020                                         *
*                                                                              *
* License   : http://www.boost.org/LICENSE_1_0.txt                             *
*******************************************************************************/

#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include "./Clipper2Lib/clipper.h"
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
    return ((clr >> 24) / 255.0f);
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
    unsigned font_color, unsigned font_size, int x, int y)
  {
      text_infos.push_back(new TextInfo(text, "", font_color, 600, font_size, x, y));
  }
  //------------------------------------------------------------------------------

  void SvgWriter::AddPath(const PathD &path, bool is_open,
    unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords)
  {
    if (path.size() == 0) return;
    PathsD p;
    p.push_back(path);
    path_infos.push_back(new PathInfo(p, is_open, 
      brush_color, pen_color, pen_width, show_coords));
  }
  //------------------------------------------------------------------------------

  void SvgWriter::AddPaths(const Paths64& paths, bool is_open, 
    unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords)
  {
    if (paths.size() == 0) return;
    PathsD tmp = Paths64ToPathsD(paths, precision);
    PathInfo* pi = new PathInfo(tmp, is_open,
      brush_color, pen_color, pen_width, show_coords);
    path_infos.push_back(pi);
  }
  //------------------------------------------------------------------------------

  void SvgWriter::AddPaths(const PathsD &paths, bool is_open,
    unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords)
  {
    if (paths.size() == 0) return;
    path_infos.push_back(new PathInfo(paths, is_open, 
      brush_color, pen_color, pen_width, show_coords));
  }
  //------------------------------------------------------------------------------

  bool SvgWriter::SaveToFile(const std::string &filename, int max_width, int max_height, int margin)
  {
    RectD rec = RectD(std::numeric_limits<double>::max(), std::numeric_limits<double>::max(), std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest());
    for (size_t i = 0; i < path_infos.size(); ++i)
      for (size_t j = 0; j < path_infos[i]->paths_.size(); ++j)
        for (size_t  k = 0; k < path_infos[i]->paths_[j].size(); ++k) {
          PointD ip = path_infos[i]->paths_[j][k];
          if (ip.x < rec.left) rec.left = ip.x;
          if (ip.x > rec.right) rec.right = ip.x;
          if (ip.y < rec.top) rec.top = ip.y;
          if (ip.y > rec.bottom) rec.bottom = ip.y;
        }

    if (rec.right <= rec.left || rec.bottom <= rec.top) return false;
    if (margin < 20) margin = 20;
    if (max_width < 100) max_width = 100;
    if (max_height < 100) max_height = 100;
    double scaleX = (double(max_width) - double(margin) * 2) / (rec.right - rec.left);
    double scaleY = (double(max_height) - double(margin) * 2) / (rec.bottom - rec.top);
    double  scale = std::min(scaleX, scaleY);

    rec.Scale(scale);
    double offsetX = -rec.left + margin;
    double offsetY = -rec.top + margin;

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
    file.precision(precision);

    for (PathInfoList::size_type i = 0; i < path_infos.size(); ++i) {
      file << "  <path d=\"";
      
      for (size_t  j = 0; j < path_infos[i]->paths_.size(); ++j) {
        size_t path_len = path_infos[i]->paths_[j].size();
        if (path_len < 2 || (path_len == 2 && !path_infos[i]->is_open_path)) continue;
        file << " M " << (static_cast<double>(path_infos[i]->paths_[j][0].x) * scale + offsetX) <<
          " " << (static_cast<double>(path_infos[i]->paths_[j][0].y) * scale + offsetY);
        for (size_t  k = 1; k < path_infos[i]->paths_[j].size(); ++k) {
          PointD ip = path_infos[i]->paths_[j][k];
          double x = ip.x * scale;
          double y = ip.y * scale;
          file << " L " << (x + offsetX) << " " << (y + offsetY);
        }
        if(!path_infos[i]->is_open_path)  file << " z";
      }
      
      file << svg_xml_0 << ColorToHtml(path_infos[i]->brush_color_) <<
        svg_xml_1 << GetAlphaAsFrac(path_infos[i]->brush_color_) <<
        svg_xml_2 <<
        (fill_rule == FillRule::EvenOdd ? "evenodd" : "nonzero") <<
        svg_xml_3 << ColorToHtml(path_infos[i]->pen_color_) <<
        svg_xml_4 << GetAlphaAsFrac(path_infos[i]->pen_color_) <<
        svg_xml_5 << path_infos[i]->pen_width_ << svg_xml_6;

      if (path_infos[i]->show_coords_) {
        file << std::setprecision(0) 
          << "  <g font-family=\"" << coords_style.font_name << "\" font-size=\"" <<
          coords_style.font_size  << "\" fill=\""<< ColorToHtml(coords_style.font_color) << 
          "\" fill-opacity=\"" << GetAlphaAsFrac(coords_style.font_color) << "\">\n";
        for (size_t  j = 0; j < path_infos[i]->paths_.size(); ++j) {
          size_t path_len = path_infos[i]->paths_[j].size();
          if (path_len < 2 || (path_len == 2 && !path_infos[i]->is_open_path)) continue;
          for (size_t  k = 0; k < path_infos[i]->paths_[j].size(); ++k) {
            PointD ip = path_infos[i]->paths_[j][k];
            file << "    <text x=\"" << static_cast<int>(ip.x * scale + offsetX) <<
              "\" y=\"" << static_cast<int>(ip.y * scale + offsetY) << "\">" <<
              ip.x << "," << ip.y << "</text>\n";
          }
        }
        file << "  </g>\n\n";
      }
    }
    
    for (TextInfoList::size_type i = 0; i < text_infos.size(); ++i) {
      file << "  <g font-family=\"" << text_infos[i]->font_name << "\" font-size=\"" <<
          text_infos[i]->font_size << "\" fill=\"" << ColorToHtml(text_infos[i]->font_color) <<
        "\" fill-opacity=\"" << GetAlphaAsFrac(text_infos[i]->font_color) << "\">\n";
      file << "    <text x=\"" << text_infos[i]->x << "\" y=\"" << text_infos[i]->y << "\">" <<
          text_infos[i]->text << "</text>\n  </g>\n\n";
    }

    file << "</svg>\n";
    file.close();
    setlocale(LC_NUMERIC, "");
    return true;
  }
  
  //------------------------------------------------------------------------------
  //------------------------------------------------------------------------------

  bool SkipBlanks(const std::string &s, size_t& pos) {
      size_t len = s.length();
      while (pos < len && s[pos] <= ' ') ++pos;
      return pos < len;
  }
  //------------------------------------------------------------------------------

  bool GetNum(const std::string &s, size_t& pos, double& value) {
      size_t len = s.length();
      while ((pos < len && (s[pos] <= ' ')) || s[pos] == ',') pos++;
      if (pos >= len) return false;
      size_t pos2 = pos;
      while ((pos < len && (s[pos] <= ' ')) || s[pos] == ',') pos++;
      bool isneg = s[pos] == '-'; 
      if (isneg) pos++;
      value = 0;
      int loop_cnt = 0, decpos = -1;
      while (pos < len) {
          if (s[pos] == '.') {
              if (loop_cnt == 0 || decpos >= 0) return false; //invalid
              else decpos = loop_cnt;
          }
          else if (s[pos] < '0' || s[pos] > '9') break;
          else value = value * 10 + (long)s[pos] - (long)'0';
          ++pos; loop_cnt++;
      }
      if (decpos >= 0) {
          decpos = loop_cnt - decpos -1;
          value *= pow(10, -decpos);
      }
      if (isneg) value = -value;
      return pos > pos2;
  }
  //------------------------------------------------------------------------------

  bool GetCommand(const std::string& s, size_t& offset, char& command, bool& is_relative) {
      if (s[offset] >= 'a' && s[offset] <= 'z') {
          is_relative = true;
          command = toupper(s[offset]);
      }
      else if (s[offset] >= 'A' && s[offset] <= 'Z') {
          command = s[offset];
          is_relative = false;
      }
      else return false; //ie leave command and is_relative unchanged!
      ++offset; //only increment the offset with a valid command
      return true;
  }

  bool SvgReader::LoadPath(const std::string& path) {
      size_t p = path.find("d=\"", 0);
      if (p == std::string::npos) return false;
      p += 3;
      if (!SkipBlanks(path, p)) return false;
      char command;
      bool is_relative;
      int vals_needed = 2;
      //nb: M == absolute move, m == relative move 
      if (!GetCommand(path, p, command, is_relative) || command != 'M') return false;
      double vals[2] = { 0,0 };
      double x = 0, y = 0;
      ++p;
      if (!GetNum(path, p, x) || !GetNum(path, p, y)) return false;
      PathsD ppp;
      PathD pp;
      pp.push_back(PointD(x, y));
      while (SkipBlanks(path, p)) {
          if (GetCommand(path, p, command, is_relative)) {
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
                  if (!GetNum(path, p, vals[i])) vals_needed = -1;
              if (vals_needed < 0) break; //oops!
              switch (vals_needed) {
              case 1: {
                  if (command == 'V') y = (is_relative ? y + vals[0] : vals[0]);
                  else x = (is_relative ? x + vals[0] : vals[0]);
                  break;
              }
              case 2: {
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
      path_infos.push_back(new PathInfo(ppp, false, 0, 0xFF000000, 1, false));
      return  (ppp.size() > 0);
  }

  bool SvgReader::LoadFromFile(const std::string &filename)
  {
      Clear();
      std::ifstream file;
      file.open(filename);      
      std::stringstream xml_buff;
      xml_buff << file.rdbuf();
      file.close();
      xml = xml_buff.str();

      size_t p = xml.find("<path", 0);
      while (p != std::string::npos) {
          p += 6;
          size_t q = xml.find("/>", p);
          if (q == std::string::npos) break;
          std::string path_str;
          path_str = xml.substr(p, q - p);
          LoadPath(path_str);
          p = xml.find("<path", q);
      }
      return path_infos.size() > 0;
  }

  void SvgReader::GetPaths(PathsD& paths) {
      paths.clear();
      for (size_t i = 0; i < path_infos.size(); ++i)
          for (size_t j = 0; j < path_infos[i]->paths_.size(); ++j)
              paths.push_back(path_infos[i]->paths_[j]);
  }

} //namespace
