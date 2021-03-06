
set(ENV{CC} "icc")
set(ENV{CXX} "icpc")
set(ENV{FC} "mpif90")

#cmake_minimum_required( VERSION 3.3.2 FATALERROR)
cmake_minimum_required( VERSION 2.8.12 )

project( wrf-jedi C CXX Fortran )

set( WRF_MODEL_PATH "/scratch3/BMC/wrfruc/mhu/code/WRF/WRFV4.0")

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)

SET(Fortran_FLAGS "-O3 -ip -fp-model precise -w -ftz -align all -fno-alias -FR -convert big_endian")
SET(Fortran_FLAGS_SIZE "  -real-size 32 -i4  ")
SET(CPP_FLAGS "-DEM_CORE=1 -DNMM_CORE=0 -DNMM_MAX_DIM=2600 -DDA_CORE=0 -DWRFPLUS=0 -DIWORDSIZE=4 -DDWORDSIZE=8 -DRWORDSIZE=4 -DLWORDSIZE=4 -DNONSTANDARD_SYSTEM_FUNC  -DWRF_USE_CLM -DUSE_NETCDF4_FEATURES -DWRFIO_NCD_LARGE_FILE_SUPPORT  -DDM_PARALLEL -DNETCDF -DPNETCDF -DUSE_ALLOCATABLES -Dwrfmodel -DGRIB1 -DINTIO -DKEEP_INT_AROUND -DLIMIT_ARGS -DBUILD_RRTMG_FAST=1 -DCONFIG_BUF_LEN=65536 -DMAX_DOMAINS_F=21 -DMAX_HISTORY=25 -DNMM_NEST=0 -DUSE_NETCDF4_FEATURES -DWRFIO_NCD_LARGE_FILE_SUPPORT")

SET(Fortran_LDFLAGS ${Fortran_FLAGS})

add_subdirectory(${PROJECT_SOURCE_DIR}/main)

