/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  1 November 2023                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2023                                         *
* Purpose   :  FAST rectangular clipping                                       *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

#ifndef CLIPPER_RECTCLIP_H
#define CLIPPER_RECTCLIP_H

#include <cstdlib>
#include <vector>
#include <queue>
#include "clipper2/clipper.core.h"

namespace Clipper2Lib
{

  enum class Location { Left, Top, Right, Bottom, Inside };

  class OutPt2;
  typedef std::vector<OutPt2*> OutPt2List;

  class OutPt2 {
  public:
    PointI pt;
    size_t owner_idx;
    OutPt2List* edge;
    OutPt2* next;
    OutPt2* prev;
  };

  //------------------------------------------------------------------------------
  // RectClipI
  //------------------------------------------------------------------------------

  class RectClipI {
  private:
    void ExecuteInternal(const PathI& path);
    PathI GetPath(OutPt2*& op);
  protected:
    const RectI rect_;
    const PathI rect_as_path_;
    const PointI rect_mp_;
    RectI path_bounds_;
    std::deque<OutPt2> op_container_;
    OutPt2List results_;  // each path can be broken into multiples
    OutPt2List edges_[8]; // clockwise and counter-clockwise
    std::vector<Location> start_locs_;
    void CheckEdges();
    void TidyEdges(int idx, OutPt2List& cw, OutPt2List& ccw);
    void GetNextLocation(const PathI& path,
      Location& loc, int& i, int highI);
    OutPt2* Add(PointI pt, bool start_new = false);
    void AddCorner(Location prev, Location curr);
    void AddCorner(Location& loc, bool isClockwise);
  public:
    explicit RectClipI(const RectI& rect) :
      rect_(rect),
      rect_as_path_(rect.AsPath()),
      rect_mp_(rect.MidPoint()) {}
    PathsI Execute(const PathsI& paths);
  };

  //------------------------------------------------------------------------------
  // RectClipLinesI
  //------------------------------------------------------------------------------

  class RectClipLinesI : public RectClipI {
  private:
    void ExecuteInternal(const PathI& path);
    PathI GetPath(OutPt2*& op);
  public:
    explicit RectClipLinesI(const RectI& rect) : RectClipI(rect) {};
    PathsI Execute(const PathsI& paths);
  };

} // Clipper2Lib namespace
#endif  // CLIPPER_RECTCLIP_H
