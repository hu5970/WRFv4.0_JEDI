!
 SUBROUTINE wrfjedi_run(head_grid)
!
   USE module_domain_type,only: domain
!   USE module_domain,only: head_grid
   USE module_domain,only: domain_clock_get,domain_clockprint,&
                           domain_get_current_time,domain_get_time_step,&
                           domain_clockisstoptime,domain_get_stop_time, &
                           WRFU_TimeInterval,WRFU_Time
   USE module_domain, only: head_grid_current_time,domain_get_current_time
   USE module_domain, only: find_grid_by_id

                           
   USE wrfjedi_module_integrate,only: wrfjedi_integrate

   IMPLICIT NONE

   type(domain), pointer :: head_grid
   type(domain), pointer :: grid
   integer :: anlGridID

!   character(len=256) :: strt_time,cur_time,stop_time,step_time
   TYPE(WRFU_TimeInterval) :: lcl_time_step
   TYPE(WRFU_Time) :: lcl_currtime,lcl_stoptime

   DO WHILE ( .NOT. domain_clockisstoptime( head_grid ) )
!
!
      lcl_time_step = domain_get_time_step(head_grid)
      lcl_currtime  = domain_get_current_time(head_grid)
      lcl_stoptime  = lcl_currtime
      lcl_stoptime%basetime%S  = lcl_currtime%basetime%S +  &
                                 lcl_time_step%basetime%S
      head_grid%start_subtime= lcl_currtime
      head_grid%stop_subtime = lcl_stoptime

      head_grid_current_time=domain_get_current_time(head_grid)

      CALL       wrf_debug ( 10 , 'wrf: calling integrate' )

      CALL wrfjedi_integrate ( 1,head_grid )

      CALL       wrf_debug ( 10 , 'wrf: back from integrate' )
   
      if(domain_clockisstoptime( head_grid )) then
         write(*,*) 'We can do data analysis here'
         call domain_clockprint(0,head_grid,'do DA') 

         CALL wrf_debug( 100 , 'DA: calling find_grid_by_id ' )
         anlGridID=1
         CALL find_grid_by_id ( anlGridID, head_grid, grid )
         write(*,*) grid%id
         grid%u_2=10.0
         CALL wrf_debug( 100 , 'DA: after calling find_grid_by_id ' )

      endif
   ENDDO

!   head_grid%start_subtime = domain_get_start_time (head_grid)
!   head_grid%stop_subtime = domain_get_stop_time (head_grid)
!   call domain_clock_get( head_grid, start_timestr=strt_time, current_timestr=cur_time)
!   call domain_clock_get( head_grid, stop_timestr=stop_time, time_stepstr=step_time)
!   write(*,*) 'check time=',trim(strt_time),'   ',trim(stop_time)
!   write(*,*) 'check time=',trim(cur_time),'   ',trim(step_time)

END SUBROUTINE wrfjedi_run
