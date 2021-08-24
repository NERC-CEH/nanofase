cmake_minimum_required(VERSION 3.13)

project(nanofase)
enable_language(Fortran)

# GFortran compiler
if (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set(common_flags "-fcheck=all -fbackslash -ffpe-trap=zero,invalid,overflow,underflow")
    set(debug_flags "-Wall -Og -g -pg ${common_flags}")
    set(release_flags "-Ofast -march=native -mtune=native ${common_flags}")
endif()

# Intel Fortran compiler
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set(common_flags "-fpe0 -check bounds -assume bscc")
    set(debug_flags "-traceback -check stack -debug full -Od ${common_flags}")
    set(release_flags "-O3 ${common_flags}")
endif()

# Find NetCDF Fortran
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" CACHE INTERNAL "Location of CMake modules.")
set(NETCDF_F90 "YES")
if(NOT DEFINED NETCDF_INCLUDES)
    find_package(NetCDF REQUIRED)
endif()

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
list(FILTER sources EXCLUDE REGEX ".*vendor/spoof/example/.*f90$")

# Modify the version number
execute_process(COMMAND git describe --tags OUTPUT_VARIABLE model_version OUTPUT_STRIP_TRAILING_WHITESPACE)
file(READ "src/VersionModule.f90" version_file)
string(REGEX REPLACE "\".*\"" "\"${model_version}\"" version_file_new "${version_file}")
file(WRITE "src/VersionModule.f90" ${version_file_new})

# Compile to executable and link to NetCDF
add_executable(nanofase ${sources})
target_include_directories(nanofase PUBLIC "${NETCDF_INCLUDES}")
target_link_libraries(nanofase PRIVATE "${NETCDF_LIBRARIES_F90}")
