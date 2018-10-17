 SUBROUTINE wrfjedi_init(head_grid)

   USE module_driver_constants
   USE module_timing
   USE module_wrf_error
   USE module_domain_type, only: domain
!   USE module_domain, only: head_grid
   USE module_domain, only: program_name,HISTORY_ALARM
   USE module_domain, only: domain_get_stop_time,domain_get_start_time,&
                            alloc_and_configure_domain,domain_get_current_time
   USE module_domain, only: head_grid_dfi_stage,head_grid_id, &
                            head_grid_current_time, &
                            head_grid_start_subtime,head_grid_stop_subtime
   USE module_configure, only : model_config_rec,grid_config_rec_type
   USE module_configure, only : set_config_as_buffer,get_config_as_buffer,&
                                initial_config,model_to_grid_config_rec
   USE module_check_a_mundo, only : setup_physics_suite,set_physics_rconfigs,&
                                    check_nml_consistency
   USE module_state_description, only: DFI_NODFI,DFI_SETUP
   USE module_symbols_util, ONLY: wrfu_cal_gregorian,WRFU_Initialize
   USE module_dm, ONLY : wrf_dm_initialize,domain_active_this_task,mpi_comm_allcompute

   USE module_bc                  , ONLY : init_module_bc
   USE module_configure           , ONLY : init_module_configure
   USE module_driver_constants    , ONLY : init_module_driver_constants
   USE module_model_constants     , ONLY : init_module_model_constants
   USE module_domain              , ONLY : init_module_domain
   USE module_machine             , ONLY : init_module_machine
   USE module_nesting             , ONLY : init_module_nesting
   USE module_timing              , ONLY : init_module_timing
   USE module_tiles               , ONLY : init_module_tiles
   USE module_io_wrf              , ONLY : init_module_io_wrf
   USE module_io                  , ONLY : init_module_io
   USE module_wrf_quilt           , ONLY : init_module_wrf_quilt
   USE module_dm                  , ONLY : init_module_dm, &
                                          split_communicator !,hwrf_coupler_init
   USE module_ext_internal        , ONLY : init_module_ext_internal
   USE module_wrf_error           , ONLY : init_module_wrf_error


   IMPLICIT NONE

!   TYPE (domain) , pointer :: wrfjedi_head_grid
   TYPE (domain) , pointer :: head_grid

   TYPE (domain) , POINTER :: null_domain
   TYPE (domain) , pointer :: parent_grid 
   TYPE (grid_config_rec_type), SAVE :: config_flags
   INTEGER        :: kid, nestid

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level

   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* 65536
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

   INTEGER :: save_comm
   logical :: no_init1
   INTEGER :: phase

   CHARACTER (LEN=10) :: release_version = 'V4.0      '

   INTERFACE 
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain, only: domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   no_init1=.true.
   program_name = "WRF " // TRIM(release_version) // " MODEL"
   
!   CALL init_modules(1)
   phase=1
   IF ( phase == 1 ) THEN
      CALL init_module_bc                ! empty
      CALL init_module_configure         ! CALL init_module_scalar_tables
      CALL init_module_driver_constants  ! empty
      CALL init_module_model_constants   ! empty
      CALL init_module_domain            ! empty
      CALL init_module_machine           ! empty

      CALL init_module_ext_internal      ! itypesize rtypesize last_next_var

      CALL split_communicator            ! frame/module_dm.f90
      CALL init_module_wrf_quilt         ! frame/module_io_quilt.f90

      CALL init_module_dm                ! frame/module_dm.f90
   ENDIF

   IF ( no_init1 ) THEN
     CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN )
   ENDIF
   
!   CALL init_modules(2)
   phase=2
   IF( phase /= 1 ) THEN
      CALL init_module_wrf_error   !  frame/module_wrf_error.f90

      CALL init_module_nesting     !  active_domain = .FALSE.
      CALL init_module_timing      !  call init_hires_timer(); cn = 0
      CALL init_module_tiles       !  empty
      CALL init_module_io_wrf      !  empty
      CALL init_module_io          !  CALL init_io_handles: wrf_io_handles(i) = -999319  

      CALL init_modules_em         !  empty
   ENDIF


   CALL wrf_get_dm_communicator( save_comm )
   CALL wrf_set_dm_communicator( mpi_comm_allcompute )
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
   CALL wrf_set_dm_communicator( save_comm )

   CALL setup_physics_suite
   CALL set_derived_rconfigs(model_config_rec)
   CALL check_nml_consistency
   CALL set_physics_rconfigs

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   NULLIFY( null_domain )

   CALL nl_get_max_dom( 1, max_dom )

   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                               active_this_task = domain_active_this_task(1), &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )
   CALL init_wrfio

   CALL wrf_get_dm_communicator( save_comm )
   CALL wrf_set_dm_communicator( mpi_comm_allcompute )
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_set_dm_communicator( save_comm )
   
   IF ( head_grid%dfi_opt .NE. DFI_NODFI ) head_grid%dfi_stage = DFI_SETUP
   head_grid_dfi_stage=head_grid%dfi_stage
   head_grid_id=head_grid%id

   CALL Setup_Timekeeping (head_grid)
   head_grid_current_time=domain_get_current_time(head_grid)

   IF ( domain_active_this_task(1) ) THEN
      CALL med_initialdata_input( head_grid , config_flags )

      IF ( config_flags%write_restart_at_0h ) THEN
         CALL med_restart_out ( head_grid, config_flags )
         CALL med_hist_out ( head_grid , HISTORY_ALARM, config_flags )
         CALL wrf_debug ( 0 , ' 0 h restart only wrf: SUCCESS COMPLETE WRF' )
         CALL wrfjedi_finalize( )
      END IF
   ENDIF  

   head_grid%start_subtime = domain_get_start_time ( head_grid )
   head_grid%stop_subtime = domain_get_stop_time ( head_grid )
   head_grid_start_subtime = domain_get_start_time ( head_grid )
   head_grid_stop_subtime = domain_get_stop_time ( head_grid )

   END SUBROUTINE wrfjedi_init


   SUBROUTINE set_derived_rconfigs(model_config_rec)

      USE module_configure, only : model_config_rec_type
      USE module_state_description, only : DFI_NODFI

      IMPLICIT NONE

      TYPE(model_config_rec_type),intent(inout) :: model_config_rec

      INTEGER :: i



      IF ( model_config_rec % dfi_opt .EQ. DFI_NODFI ) THEN
        DO i = 1, model_config_rec % max_dom
           model_config_rec % mp_physics_dfi(i) = -1
        ENDDO
      ELSE
        DO i = 1, model_config_rec % max_dom
           model_config_rec % mp_physics_dfi(i) = model_config_rec % mp_physics(i)
        ENDDO
      END IF


      IF ( model_config_rec % dfi_opt .EQ. DFI_NODFI ) THEN
        DO i = 1, model_config_rec % max_dom
           model_config_rec % bl_pbl_physics_dfi(i) = -1
        ENDDO
      ELSE
        DO i = 1, model_config_rec % max_dom
           model_config_rec % bl_pbl_physics_dfi(i) = model_config_rec % bl_pbl_physics(i)
        ENDDO
      END IF


   END SUBROUTINE set_derived_rconfigs
