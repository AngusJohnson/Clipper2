# Debian Packaging for Clipper2

This document describes how to build and use Debian packages for the Clipper2 library.

## Overview

Clipper2 includes complete Debian packaging infrastructure in the `debian/` directory, allowing you to build professional-quality Debian packages locally or for distribution.

## Generated Packages

### Runtime Package: `libclipper2-1`
- **Contains:** Shared libraries for runtime use
- **Libraries:**
  - `libClipper2.so.1.5.4` - Standard polygon clipping library
  - `libClipper2Z.so.1.5.4` - Z-coordinate variant for 3D applications
- **Size:** ~100KB
- **Dependencies:** Automatically detected (libc6, libgcc-s1, libstdc++6)

### Development Package: `libclipper2-dev`
- **Contains:** Headers, CMake config, and build tools
- **Includes:**
  - Header files in `/usr/include/clipper2/`
  - CMake configuration files for `find_package(Clipper2)`
  - pkg-config files for both Clipper2 and Clipper2Z
  - Development symlinks
- **Size:** ~28KB
- **Dependencies:** libclipper2-1 (= exact version)

## Building Packages

### Prerequisites
```bash
# Install build dependencies
sudo apt update
sudo apt install debhelper-compat cmake pkg-config build-essential
```

### Build Commands
```bash
# Build unsigned packages (for local use)
dpkg-buildpackage -us -uc

# Alternative using debuild
debuild -us -uc

# For signed packages (for distribution)
dpkg-buildpackage
```

### Build Options
The packaging automatically configures the build with:
- `BUILD_SHARED_LIBS=ON` - Creates shared libraries
- `CLIPPER2_EXAMPLES=OFF` - Excludes examples from packages
- `CLIPPER2_TESTS=OFF` - Excludes tests from packages
- `CLIPPER2_UTILS=OFF` - Excludes utility libraries from packages

## Installation

### Install Built Packages
```bash
# Install both packages
sudo dpkg -i ../libclipper2-1_*.deb ../libclipper2-dev_*.deb

# Fix any dependency issues
sudo apt-get install -f
```

### Uninstall
```bash
sudo apt remove libclipper2-dev libclipper2-1
```

## Using the Installed Library

### With CMake (Recommended)
```cmake
cmake_minimum_required(VERSION 3.15)
project(MyProject)

# Find the library
find_package(Clipper2 REQUIRED)

# Create your executable
add_executable(myapp main.cpp)

# Link against Clipper2
target_link_libraries(myapp Clipper2::Clipper2)

# For Z-coordinate support:
# target_link_libraries(myapp Clipper2::Clipper2Z)
```

### With pkg-config
```bash
# Compile with standard Clipper2
g++ -o myapp main.cpp $(pkg-config --cflags --libs Clipper2)

# Compile with Z-coordinate support
g++ -o myapp main.cpp $(pkg-config --cflags --libs Clipper2Z)

# Check available flags
pkg-config --list-all | grep -i clipper
```

### Manual Compilation
```bash
# Standard library
g++ -o myapp main.cpp -lClipper2

# Z-coordinate variant
g++ -o myapp main.cpp -lClipper2Z

# With explicit include path (usually not needed)
g++ -I/usr/include/clipper2 -o myapp main.cpp -lClipper2
```

### Simple Usage Example
```cpp
#include <clipper2/clipper.h>
#include <iostream>

using namespace Clipper2Lib;

int main() {
    Paths64 subject, clip, solution;

    subject.push_back(MakePath({100, 50, 10, 79, 65, 2, 65, 98, 10, 21}));
    clip.push_back(MakePath({98, 63, 4, 68, 77, 8, 52, 100, 19, 12}));

    solution = Intersect(subject, clip, FillRule::NonZero);

    std::cout << "Intersection found " << solution.size() << " paths" << std::endl;
    return 0;
}
```

## Package Quality

### Standards Compliance
- **Debian Policy:** 4.6.2 compliant
- **debhelper:** Modern compat level 13
- **Multi-arch:** Full `Multi-Arch: same` support
- **Lintian:** Clean with only expected warnings for new packages

### Features
- ✅ Automatic dependency detection
- ✅ Proper library versioning and symlinks
- ✅ CMake and pkg-config integration
- ✅ Multi-architecture support
- ✅ Debug symbol packages (automatically generated)
- ✅ Hardening flags enabled

## Troubleshooting

### Build Issues
```bash
# Check build dependencies
dpkg-checkbuilddeps

# Clean build environment
debian/rules clean

# Verbose build
DEB_BUILD_OPTIONS=verbose dpkg-buildpackage -us -uc
```

### Installation Issues
```bash
# Check package contents
dpkg -L libclipper2-dev
dpkg -c ../libclipper2-1_*.deb

# Verify library installation
ldconfig -p | grep -i clipper

# Check CMake detection
cmake --find-package -DNAME=Clipper2 -DCOMPILER_ID=GNU -DLANGUAGE=CXX
```

### Runtime Issues
```bash
# Check library dependencies
ldd /usr/lib/x86_64-linux-gnu/libClipper2.so.1

# Verify pkg-config
pkg-config --modversion Clipper2
pkg-config --cflags Clipper2
```

## Package Files Structure

```
debian/
├── changelog          # Version history
├── control           # Package metadata and dependencies
├── copyright         # License information
├── rules             # Build rules (executable)
├── source/
│   └── format        # Source format specification
├── libclipper2-1.install     # Runtime files list
├── libclipper2-dev.install   # Development files list
└── README.Debian     # Package documentation
```

## Contributing

### Updating Package Version
1. Update version in `debian/changelog`
2. Ensure `CPP/CMakeLists.txt` version matches
3. Test build with new version
4. Update any version-specific references

### Package Maintenance
- Follow [Debian Policy](https://www.debian.org/doc/debian-policy/)
- Test with `lintian` before releasing
- Maintain compatibility with supported Debian/Ubuntu versions
- Keep build dependencies minimal

## License

The Debian packaging files are released under the same Boost Software License 1.0 as the upstream Clipper2 library.