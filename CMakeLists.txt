cmake_minimum_required(VERSION 3.13)

project(nanofase)
enable_language(Fortran)

if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(debug_flags "-Wall -O0 -fcheck=all -fbackslash -g -ffpe-trap=zero,invalid,overflow,underflow -pg")
    set(release_flags "-O3 -fbackslash")
endif()

if (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(debug_flags "/assume:bscc /fpe:0 /check:bounds /traceback /check:stack /debug:full /Od")
    set(release_flags "/O3 /assume:bscc")
endif()

# Get the NetCDF flags by running nf-config command
execute_process(COMMAND nf-config --fflags --flibs OUTPUT_VARIABLE netcdf_flags)
# For some reason nf-config returns string with newline characters, so we need to remove these
string(REPLACE "\n" " " netcdf_flags ${netcdf_flags})

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${debug_flags} ${netcdf_flags}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${release_flags} ${netcdf_flags}")
# set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${netcdf_flags}")


file(GLOB_RECURSE sources src/*.f90 vendor/*.f90)
list(FILTER sources EXCLUDE REGEX ".*vendor/feh/example/.*f90$")
list(FILTER sources EXCLUDE REGEX ".*vendor/datetime-fortran/src/tests/datetime_tests.f90$")
list(FILTER sources EXCLUDE REGEX ".*vendor/mo_netcdf/examples/.*f90$")
list(FILTER sources EXCLUDE REGEX ".*vendor/mo_netcdf/tests/.*f90$")
add_executable(prog ${sources})
target_link_options(prog INTERFACE "${netcdf_flags}")