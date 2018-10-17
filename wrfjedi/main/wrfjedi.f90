PROGRAM wrf

   USE module_domain_type,   only : domain

   IMPLICIT NONE
   TYPE (domain), pointer    :: wrfjedi_head_grid=>NULL()
  
   INTERFACE

     SUBROUTINE wrfjedi_init(wrfjedi_head_grid)
        USE module_domain_type,   only : domain
        TYPE(domain), pointer :: wrfjedi_head_grid
     END SUBROUTINE wrfjedi_init

     SUBROUTINE wrfjedi_run(wrfjedi_head_grid)
        USE module_domain_type,   only : domain
        TYPE(domain), pointer :: wrfjedi_head_grid
     END SUBROUTINE wrfjedi_run

     SUBROUTINE wrfjedi_finalize(wrfjedi_head_grid)
        USE module_domain_type,   only : domain
        TYPE(domain), pointer :: wrfjedi_head_grid
     END SUBROUTINE wrfjedi_finalize
   END INTERFACE
!
  CALL wrfjedi_init(wrfjedi_head_grid)
  
!  CALL wrf_dfi
  
  CALL wrfjedi_run(wrfjedi_head_grid)

  CALL wrfjedi_finalize(wrfjedi_head_grid)

END PROGRAM wrf
