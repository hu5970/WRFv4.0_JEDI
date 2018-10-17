SET(SRC_LIST wrfjedi.f90 wrfjedi_init.f90 wrfjedi_run.f90 wrfjedi_finalize.f90 wrfjedi_module_integrate.f90)
#SET(SRC_LIST wrfjedi.f90 module_wrf_top.f90 wrfjedi_run.f90 wrfjedi_finalize.f90)
SET(WRFJEDIEXE wrfjedi.exe)

set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS "${Fortran_FLAGS} ${Fortran_FLAGS_SIZE} ${CPP_FLAGS}")

include_directories ("${WRF_MODEL_PATH}/external/esmf_time_f90")
include_directories ("${WRF_MODEL_PATH}/external/io_int")
include_directories ("${WRF_MODEL_PATH}/main")
include_directories ("${WRF_MODEL_PATH}/frame")
include_directories ("${WRF_MODEL_PATH}/share")

ADD_EXECUTABLE(${WRFJEDIEXE} ${SRC_LIST})

set(WRF_LIBS)
SET(WRF_LIBS_EXTERNAL ${WRF_MODEL_PATH}/external/fftpack/fftpack5/libfftpack.a ${WRF_MODEL_PATH}/external/io_grib1/libio_grib1.a ${WRF_MODEL_PATH}/external/io_grib_share/libio_grib_share.a ${WRF_MODEL_PATH}/external/io_int/libwrfio_int.a ${WRF_MODEL_PATH}/external/esmf_time_f90/libesmf_time.a ${WRF_MODEL_PATH}/external/RSL_LITE/librsl_lite.a)
SET(WRF_LIBS_NF -L${WRF_MODEL_PATH}/external/io_netcdf -lwrfio_nf -L/apps/netcdf/4.3.0-intel/lib -lnetcdff -lnetcdf -L${WRF_MODEL_PATH}/external/io_pnetcdf -lwrfio_pnf -L/apps/pnetcdf/1.5.0-intel-mvapich2/lib -lpnetcdf)

#list(APPEND WRF_LIBS ${WRF_MODEL_PATH}/main/module_wrf_top.o -L${WRF_MODEL_PATH}/main -lwrflib)
list(APPEND WRF_LIBS -L${WRF_MODEL_PATH}/main -lwrflib)
list(APPEND WRF_LIBS ${WRF_LIBS_EXTERNAL})
list(APPEND WRF_LIBS ${WRF_MODEL_PATH}/frame/module_internal_header_util.o ${WRF_MODEL_PATH}/frame/pack_utils.o ${WRF_LIBS_NF})

set_target_properties(${WRFJEDIEXE} PROPERTIES LINK_FLAGS "${Fortran_LDFLAGS}")
TARGET_LINK_LIBRARIES(${WRFJEDIEXE} ${WRF_LIBS})

