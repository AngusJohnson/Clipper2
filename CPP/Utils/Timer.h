#ifndef CLIPPER_TIMER_H
#define CLIPPER_TIMER_H

#include <cstdlib>
#include <string>
#include <chrono> 
#include <iomanip>

/*

Timer Usage:
The Timer object will start immediately following its construction 
(unless "start_paused" is true). It will stop either when its 
destructor is called on leaving scope, or when pause() is called.
The timer's pause() and resume() can be called an number of times and 
the total interval while unpaused will be reported in standard output.

The Timer's constructor takes two optional string parameters:
  caption   : test sent to standard output just as timing commences.
  time_text : text to be output to the left of the recorded time interval.

Example:

  #include "timer.h"
  #include "windows.h" //for Sleep() :)

  void main()
  {
    {
      Timer timer("Starting timer now.", "This operation took ");
      Sleep(1000);
    }
  }

*/

struct Timer {
private:
  std::streamsize old_precision = std::cout.precision(0);
  std::ios_base::fmtflags old_flags = std::cout.flags();
  std::chrono::high_resolution_clock::time_point time_started_ = 
    std::chrono::high_resolution_clock::now();
  std::chrono::high_resolution_clock::duration duration_ = {};
  bool paused_ = false;
  std::string time_text_ = "";

public:

  Timer(bool start_paused = false): paused_(start_paused) {}

  explicit Timer(const char time_text[], bool start_paused) :
    paused_(start_paused), time_text_(time_text) {}

  explicit Timer(const char caption[], const char time_text[] = "",
    bool start_paused = false) :
    paused_(start_paused), time_text_(time_text)
  {
    std::cout << caption << std::endl;
  }

  explicit Timer(const std::string& caption, const std::string& time_text = "",
    bool start_paused = false) :
    paused_(start_paused), time_text_(time_text)
  {
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
