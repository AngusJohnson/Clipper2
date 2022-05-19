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
  int old_flags;
  std::chrono::steady_clock::time_point time_started = {};
  std::string _time_text = "";  
  void init() 
  { 
    old_precision = std::cout.precision(0);
    old_flags = std::cout.flags();
    time_started = std::chrono::high_resolution_clock::now(); 
  }
public:
  explicit Timer() { init(); }  
  explicit Timer(const std::string& caption, const std::string& time_text = "")  
  {
    init();
    _time_text = time_text;
    if (caption != "") std::cout << caption << std::endl;
  }

  ~Timer()
  {
    std::chrono::steady_clock::time_point 
      time_ended = std::chrono::high_resolution_clock::now();
    int nsecs = (int)std::log10(std::chrono::duration_cast<std::chrono::nanoseconds>
      (time_ended - time_started).count());

    std::cout << std::fixed << std::setprecision((uint8_t)(2 -(nsecs % 3))) << _time_text;
    if (nsecs < 6) 
      std::cout << std::chrono::duration_cast<std::chrono::nanoseconds>
      (time_ended - time_started).count() * 1.0e-3 << " microsecs" << std::endl;
    else if (nsecs < 9)
      std::cout << std::chrono::duration_cast<std::chrono::microseconds>
        (time_ended - time_started).count() * 1.0e-3 << " millisecs" << std::endl;
    else 
      std::cout << std::chrono::duration_cast<std::chrono::milliseconds>
      (time_ended - time_started).count() * 1.0e-3 << " secs" << std::endl;
    
    std::cout.precision(old_precision);
    std::cout.flags(old_flags);
  }
};

#endif CLIPPER_TIMER_H