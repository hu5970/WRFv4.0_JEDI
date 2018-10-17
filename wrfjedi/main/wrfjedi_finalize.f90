SUBROUTINE wrfjedi_finalize(head_grid)
!
   USE module_domain,   only : domain
   USE module_configure,only : grid_config_rec_type,model_to_grid_config_rec,&
                               model_config_rec
   USE module_utility,  only : WRFU_finalize

   IMPLICIT NONE
   LOGICAL  :: no_shutdown
   TYPE (domain) , POINTER   :: head_grid
   TYPE (grid_config_rec_type)  :: config_flags
!
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , &
                                   config_flags )
!
   CALL med_shutdown_io ( head_grid , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )
   CALL       wrf_debug (   0 , 'wrf: SUCCESS COMPLETE WRF' )

   no_shutdown=.true.
   IF ( no_shutdown  ) THEN
      CALL WRFU_Finalize
      CALL wrf_shutdown
   ENDIF

END SUBROUTINE wrfjedi_finalize
