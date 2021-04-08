cmake_minimum_required(VERSION 3.13)

project(nanofase)
enable_language(Fortran)

# GFortran compiler
if (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set(debug_flags "-Wall -O1 -fcheck=all -fbackslash -g -ffpe-trap=zero,invalid,overflow,underflow -pg")
    set(release_flags "-O3 -fbackslash")
endif()

# Intel Fortran compiler
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set(debug_flags "/assume:bscc /fpe:0 /check:bounds /traceback /check:stack /debug:full /Od")
    set(release_flags "/O3 /assume:bscc")
endif()

# Find NetCDF Fortran
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" CACHE INTERNAL "Location of CMake modules.")
set(NETCDF_F90 "YES")
find_package(NetCDF REQUIRED)

# Set debug and release flags
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${debug_flags}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${release_flags}")

# Sources to compile - everything except those matching the regex
file(GLOB_RECURSE sources src/*.f90 vendor/*.f90)
list(FILTER sources EXCLUDE REGEX ".*vendor/feh/example/.*f90$")
list(FILTER sources EXCLUDE REGEX ".*vendor/feh/tests/.*f90$")
list(FILTER sources EXCLUDE REGEX ".*vendor/datetime-fortran/src/tests/.*f90$")
list(FILTER sources EXCLUDE REGEX ".*vendor/mo_netcdf/examples/.*f90$")
list(FILTER sources EXCLUDE REGEX ".*vendor/mo_netcdf/tests/.*f90$")

# Compile to executable and link to NetCDF
add_executable(nanofase ${sources})
target_include_directories(nanofase PUBLIC "${NETCDF_INCLUDES}")
target_link_libraries(nanofase PRIVATE "${NETCDF_LIBRARIES_F90}")