#ifndef CLIPPER_TIMER_H
#define CLIPPER_TIMER_H

#include <cstdlib>
#include <string>
#include <chrono> 
#include <iomanip>
#include <sstream>

/*

Timer Usage:
The Timer object will start immediately following its construction 
(unless "start_paused" is true). The timer's pause() and resume() can 
be called an number of times and the total interval while unpaused 
will be returned when elapsed() is called.

Example:

  #include "timer.h"

  void main()
  {
    Timer timer;
    Sleep(1000);
    std::cout << timer.elapsed_str();
  }

*/

struct Timer {
private:
  std::streamsize old_precision = std::cout.precision(0);
  std::ios_base::fmtflags old_flags = std::cout.flags();
  std::chrono::high_resolution_clock::time_point time_started_;
  std::chrono::high_resolution_clock::duration duration_ = {};
  bool paused_ = false;

public:

  Timer(bool start_paused = false): paused_(start_paused) 
  {
    if (!paused_) time_started_ =
      std::chrono::high_resolution_clock::now();
  }

  void restart()
  {
    paused_ = false;
    duration_ = {};
    time_started_ = std::chrono::high_resolution_clock::now();
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

  int64_t elapsed_nano() // result in nano-seconds
  {
    if (!paused_)
    {
      std::chrono::high_resolution_clock::time_point now =
        std::chrono::high_resolution_clock::now();
      duration_ += (now - time_started_);
    }
    return std::chrono::duration_cast<std::chrono::nanoseconds>(duration_).count();
  }

  std::string elapsed_str() // result as string
  {
    int64_t nano_secs = elapsed_nano();
    int nsecs_log10 = static_cast<int>(std::log10(nano_secs));
    std::ostringstream os{};
    os.precision(static_cast<uint8_t>(2.0 - (nsecs_log10 % 3)));
    os << std::fixed;
    if (nsecs_log10 < 6)
      os << nano_secs * 1.0e-3 << " microsecs";
    else if (nsecs_log10 < 9)
      os << nano_secs * 1.0e-6 << " millisecs";
    else
      os << nano_secs * 1.0e-9 << " secs";
    return os.str();
  }
};

#endif // CLIPPER_TIMER_H
