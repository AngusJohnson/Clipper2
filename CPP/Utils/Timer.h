#ifndef CLIPPER_TIMER_H
#define CLIPPER_TIMER_H

#include <cstdlib>
#include <string>
#include <chrono> 
#include <iomanip>

/*

Timer Usage:

The Timer object will start immediately following its construction.
It will stop when its destructor is called on leaving scope and
the time interval will be sent to standard output.

The Timer's constructor takes two optional string parameters:
  caption   : test sent to standard output just before timing commences.
  time_text : text to be output to the left of the recorded time interval.

Example:

  #include "timer.h"
  #include "windows.h" //for Sleep() :)

  void main()
  {

    //when this code block finishes, the time interval 
    //will be sent to standard output.
    {
      Timer timer("Starting timer now.", "This operation took ");
      Sleep(1000);
    }

  }

*/

struct Timer {
private:
  std::streamsize old_precision;
  std::ios_base::fmtflags old_flags;
  bool paused_ = false;
  std::chrono::high_resolution_clock::time_point time_started_ = {};
  std::chrono::high_resolution_clock::duration duration_ = {};
  std::string time_text_ = "";
  
  void init(bool start_paused)
  { 
    old_precision = std::cout.precision(0);
    old_flags = std::cout.flags();
    paused_ = start_paused;
    if (!start_paused)
      time_started_ = std::chrono::high_resolution_clock::now();
  }

public:
  explicit Timer(bool start_paused = false)
  { 
    init(start_paused);
  }
  explicit Timer(const std::string& caption,
    const std::string& time_text = "", bool start_paused = false)  
  {
    init(start_paused);
    time_text_ = time_text;
    if (caption != "") std::cout << caption << std::endl;
  }

  void resume()
  {
    if (!paused_) return;
    paused_ = false;
    time_started_ = std::chrono::high_resolution_clock::now();
  }

  void pause()
  {
    if (paused_) return;
    std::chrono::high_resolution_clock::time_point now =
      std::chrono::high_resolution_clock::now();
    duration_ += (now - time_started_);
    paused_ = true;
  }

  ~Timer()
  {
    pause();
    int nsecs_log10 = static_cast<int>(std::log10(
      std::chrono::duration_cast<std::chrono::nanoseconds>(duration_).count()));
    std::cout << std::fixed << 
      std::setprecision(static_cast<uint8_t>(2 -(nsecs_log10 % 3))) << time_text_;
    if (nsecs_log10 < 6) 
      std::cout << std::chrono::duration_cast<std::chrono::nanoseconds>
      (duration_).count() * 1.0e-3 << " microsecs" << std::endl;
    else if (nsecs_log10 < 9)
      std::cout << std::chrono::duration_cast<std::chrono::microseconds>
        (duration_).count() * 1.0e-3 << " millisecs" << std::endl;
    else 
      std::cout << std::chrono::duration_cast<std::chrono::milliseconds>
      (duration_).count() * 1.0e-3 << " secs" << std::endl;
    std::cout.precision(old_precision);
    std::cout.flags(old_flags);
  }
};

#endif // CLIPPER_TIMER_H
