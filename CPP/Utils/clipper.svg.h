/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  24 March 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef svglib_h
#define svglib_h

#include <cstdlib>
#include <string>
#include "clipper2/clipper.h"

namespace Clipper2Lib {

    class PathInfo {
    private:
        PathsD paths_;
        bool is_open_path;
        FillRule fillrule_;
        unsigned brush_color_;
        unsigned pen_color_;
        double pen_width_;
        bool show_coords_;

    public:
        PathInfo(const PathsD& paths, bool is_open, FillRule fillrule,
            unsigned brush_clr, unsigned pen_clr, double pen_width, bool show_coords) :
            paths_(paths), is_open_path(is_open), fillrule_(fillrule),
            brush_color_(brush_clr), pen_color_(pen_clr),
            pen_width_(pen_width), show_coords_(show_coords) {};
        friend class SvgWriter;
        friend class SvgReader;
    };
    typedef std::vector< PathInfo* > PathInfoList;

  //---------------------------------------------------------------------------
  // SvgWriter
  //---------------------------------------------------------------------------

  class SvgWriter
  {
    class CoordsStyle {
    public:
      std::string font_name;
      unsigned font_color = 0xFF000000;
      unsigned font_size = 7;
    };

    class TextInfo {
    public:
        std::string text;
        std::string font_name;
        unsigned font_color = 0xFF000000;
        unsigned font_weight = 600;
        unsigned font_size = 11;
        double x = 0;
        double y = 0;

        TextInfo(const std::string &txt, const std::string &fnt_name, unsigned color,
            unsigned weight, unsigned size, double coord_x, double coord_y) :
            text(txt), font_name(fnt_name), font_color(color), font_weight(weight), font_size(size),
            x(coord_x), y(coord_y) {};
        friend class SvgWriter;
    };

    typedef std::vector< TextInfo* > TextInfoList;

  private:
      double scale_;
      FillRule fill_rule_;
      CoordsStyle coords_style;
      TextInfoList text_infos;
      PathInfoList path_infos;
      void DrawCircle(std::ofstream& file, double x, double y, double radius);
  public:
    explicit SvgWriter(int precision = 0)
    {
      fill_rule_ = FillRule::NonZero;
      coords_style.font_name = "Verdana";
      coords_style.font_color = 0xFF000000;
      coords_style.font_size = 11;
      scale_ = std::pow(10, precision);
    };

    ~SvgWriter() { Clear(); };

    void Clear();
    FillRule Fill_Rule() { return fill_rule_; }
    void SetCoordsStyle(const std::string &font_name, unsigned font_color, unsigned font_size);
    void AddText(const std::string &text, unsigned font_color, unsigned font_size, double x, double y);
    void AddPath(const Path64& path, bool is_open, FillRule fillrule,
      unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords);
    void AddPath(const PathD& path, bool is_open, FillRule fillrule,
      unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords);
    void AddPaths(const PathsD& paths, bool is_open, FillRule fillrule,
      unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords);
    void AddPaths(const Paths64& paths, bool is_open, FillRule fillrule,
      unsigned brush_color, unsigned pen_color, double pen_width, bool show_coords);
    bool SaveToFile(const std::string &filename, int max_width, int max_height, int margin);
  };

  //---------------------------------------------------------------------------
  // SvgReader
  //---------------------------------------------------------------------------

  class SvgReader
  {
  private:
      PathInfoList path_infos;
      bool LoadPath(std::string::const_iterator& p,
        const std::string::const_iterator& pe);
  public:
      std::string xml;
      bool LoadFromFile(const std::string &filename);
      void Clear() { path_infos.clear(); };
      PathsD GetPaths();
  };

}

#endif //svglib_h
