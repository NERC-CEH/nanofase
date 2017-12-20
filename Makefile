# Modules in compilation order.
modules = vendor/feh/src/ErrorInstance.f08 \
	vendor/feh/src/Result.f08 \
	vendor/feh/src/ErrorHandler.f08 \
	vendor/feh/src/ErrorCriteria.f08 \
	vendor/mo_netcdf/src/mo_types.f90 \
	vendor/mo_netcdf/src/mo_netcdf.f90 \
	src/Globals.f08 \
	src/UtilModule.f08 \
	src/Biota/spcBiota.f08 \
	src/Biota/classBiota1.f08 \
	src/Biota/classBiota2.f08 \
	src/Reactor/spcReactor.f08 \
	src/Reactor/classReactor1.f08 \
	src/Reactor/classReactor2.f08 \
	src/BedSedimentLayer/classFineSediment1.f08\
	src/BedSedimentLayer/spcBedSedimentLayer.f08\
	src/BedSedimentLayer/classBedSedimentLayer1.f08\
	src/BedSediment/spcBedSediment.f08\
	src/BedSediment/classBedSediment1.f08\
	src/River/RiverReach/spcRiverReach.f08 \
	src/River/RiverReach/classRiverReach1.f08 \
	src/River/SubRiver/spcSubRiver.f08 \
	src/River/SubRiver/classSubRiver1.f08 \
	src/Soil/spcSoilLayer.f08 \
	src/Soil/classSoilLayer1.f08 \
	src/Soil/spcSoilProfile.f08 \
	src/Soil/classSoilProfile1.f08 \
	src/Source/classDiffuseSource.f08 \
	src/Source/classPointSource.f08 \
	src/GridCell/spcGridCell.f08 \
	src/GridCell/classGridCell1.f08 \
	src/Environment/spcEnvironment.f08 \
	src/Environment/classEnvironment1.f08 \
	src/main.f08

# Where should the output .exe be placed and run?
outputFile = S:/ECP/CPG/model_FORTRAN/exec/fprog_source_sl
flags = -g -O1 -fcheck=all -fbackslash -Wall -Wno-unused-dummy-argument
netcdf = `nf-config --fflags --flibs`

# The commands:
# 	make main: Compile and create exe with default errors.
# 	make compile: Only compile, don't create exe.
# 	make run: Same as `make main`, but run the exe as well.
# 	make j2n: Convert the data.json file to data.nc in root directory.
# 	make clean: Remove compiled files.
# 	
# 	-O0 flag would be preferable but a GFortran bug causes character
# 	length mismatch in array constructor errors with allocatable
# 	character variables, even when they're the correct length. See
# 	https://gcc.gnu.org/bugzilla/show_bug.cgi?id=70231 and
# 	https://stackoverflow.com/questions/44385909/adding-to-an-array-of-characters-in-fortran.
# 	Solution is to use -O1 or higher.
main: $(modules)
	gfortran -Ivendor/json-fortran/lib $(vendor) $(modules) $(flags)  -o "$(outputFile)" $(netcdf)
run: $(modules)
	gfortran -Ivendor/json-fortran/lib $(vendor) $(modules) $(flags) -o "$(outputFile)" $(netcdf) && "$(outputFile)"
j2n:
	python vendor/json2netcdf/json2netcdf.py data/data.json data/data.nc
clean:
	rm -f *.mod *.o *.stackdump