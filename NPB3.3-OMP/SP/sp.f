!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.3         !
!                                                                         !
!                       O p e n M P     V E R S I O N                     !
!                                                                         !
!                                   S P                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is an OpenMP version of the NPB SP code.              !
!    It is described in NAS Technical Report 99-011.                      !
!                                                                         !
!    Permission to use, copy, distribute and modify this software         !
!    for any purpose with or without fee is hereby granted.  We           !
!    request, however, that all derived work reference the NAS            !
!    Parallel Benchmarks 3.3. This software is provided "as is"           !
!    without express or implied warranty.                                 !
!                                                                         !
!    Information on NPB 3.3, including the technical report, the          !
!    original specifications, source code, results and information        !
!    on how to submit new results, is available at:                       !
!                                                                         !
!           http://www.nas.nasa.gov/Software/NPB/                         !
!                                                                         !
!    Send comments or suggestions to  npb@nas.nasa.gov                    !
!                                                                         !
!          NAS Parallel Benchmarks Group                                  !
!          NASA Ames Research Center                                      !
!          Mail Stop: T27A-1                                              !
!          Moffett Field, CA   94035-1000                                 !
!                                                                         !
!          E-mail:  npb@nas.nasa.gov                                      !
!          Fax:     (650) 604-3957                                        !
!                                                                         !
!-------------------------------------------------------------------------!

c---------------------------------------------------------------------
c
c Authors: R. Van der Wijngaart
c          W. Saphir
c          H. Jin
c---------------------------------------------------------------------
        MODULE numa
        use iso_c_binding
        IMPLICIT none
       INTERFACE
       FUNCTION numa_alloc(s, n) BIND(C,NAME='numa_alloc_onnode')
                IMPORT :: C_PTR
                TYPE(C_PTR) :: numa_alloc
                INTEGER(8),VALUE :: s
                INTEGER(4),VALUE :: n
        END FUNCTION
        END INTERFACE
        INTERFACE
        FUNCTION numa_available() BIND(C, NAME='numa_available')
                TYPE(INTEGER) :: numa_available
        END FUNCTION
        END INTERFACE
        INTERFACE
        FUNCTION numa_max_node() BIND(C, NAME='numa_max_node')
                TYPE(INTEGER) :: numa_max_node
        END FUNCTION
        END INTERFACE
        INTERFACE
        SUBROUTINE numa_free(p,s) BIND(C, NAME='numa_free')
                IMPORT :: C_PTR
                TYPE(C_PTR) :: p
                INTEGER(8),VALUE :: s
        END SUBROUTINE
        END INTERFACE
       end module numa
c---------------------------------------------------------------------
       program SP
        use iso_c_binding
        use numa
c---------------------------------------------------------------------

       include  'header.h'

       double precision, pointer :: 
     >   us      (:,:,:),
     >   vs      (:,:,:),
     >   ws      (:,:,:),
     >   qs      (:,:,:),
     >   rho_i   (:,:,:),
     >   square  (:,:,:),
     >   speed   (:,:,:),
     >   forcing (:,:,:,:),
     >   u       (:,:,:,:),
     >   rhs     (:,:,:,:)
      
      integer(8) us_s,vs_s,ws_s,qs_s,rho_i_s,
     >  square_s,forcing_s,u_s,rhs_s,speed_s
        type(c_ptr) :: usptr,vsptr,wsptr,qsptr,rho_iptr,
     >  squareptr,forcingptr, uptr,rhsptr,speedptr

       integer          i, niter, step, fstatus, n3
       external         timer_read
       double precision mflops, t, tmax, timer_read, trecs(t_last)
       logical          verified
       character        class
       character        t_names(t_last)*8
!$     integer  omp_get_max_threads
!$     external omp_get_max_threads

c---------------------------------------------------------------------
c      Read input file (if it exists), else take
c      defaults from parameters
c---------------------------------------------------------------------
          
       open (unit=2,file='timer.flag',status='old', iostat=fstatus)
       if (fstatus .eq. 0) then
         timeron = .true.
         t_names(t_total) = 'total'
         t_names(t_rhsx) = 'rhsx'
         t_names(t_rhsy) = 'rhsy'
         t_names(t_rhsz) = 'rhsz'
         t_names(t_rhs) = 'rhs'
         t_names(t_xsolve) = 'xsolve'
         t_names(t_ysolve) = 'ysolve'
         t_names(t_zsolve) = 'zsolve'
         t_names(t_rdis1) = 'redist1'
         t_names(t_rdis2) = 'redist2'
         t_names(t_tzetar) = 'tzetar'
         t_names(t_ninvr) = 'ninvr'
         t_names(t_pinvr) = 'pinvr'
         t_names(t_txinvr) = 'txinvr'
         t_names(t_add) = 'add'
         close(2)
       else
         timeron = .false.
       endif

       write(*, 1000)
       open (unit=2,file='inputsp.data',status='old', iostat=fstatus)

       if (fstatus .eq. 0) then
         write(*,233) 
 233     format(' Reading from input file inputsp.data')
         read (2,*) niter
         read (2,*) dt
         read (2,*) grid_points(1), grid_points(2), grid_points(3)
         close(2)
       else
         write(*,234) 
         niter = niter_default
         dt    = dt_default
         grid_points(1) = problem_size
         grid_points(2) = problem_size
         grid_points(3) = problem_size
       endif
 234   format(' No input file inputsp.data. Using compiled defaults')

       write(*, 1001) grid_points(1), grid_points(2), grid_points(3)
       write(*, 1002) niter, dt
!$     write(*, 1003) omp_get_max_threads()
       write(*, *)

 1000  format(//, ' NAS Parallel Benchmarks (NPB3.3-OMP)',
     >            ' - SP Benchmark', /)
 1001  format(' Size: ', i4, 'x', i4, 'x', i4)
 1002  format(' Iterations: ', i4, '    dt:  ', F11.7)
 1003  format(' Number of available threads: ', i5)

       if ( (grid_points(1) .gt. IMAX) .or.
     >      (grid_points(2) .gt. JMAX) .or.
     >      (grid_points(3) .gt. KMAX) ) then
             print *, (grid_points(i),i=1,3)
             print *,' Problem size too big for compiled array sizes'
             goto 999
       endif
       nx2 = grid_points(1) - 2
       ny2 = grid_points(2) - 2
       nz2 = grid_points(3) - 2

       call set_constants
c--------------------------------------------------------------------
c      Allocate
c--------------------------------------------------------------------
        us_s = int((IMAXP+1),8)*(JMAXP+1)*(KMAX)*16
        vs_s = int((IMAXP+1),8)*(JMAXP+1)*(KMAX)*16
        ws_s = int((IMAXP+1),8)*(JMAXP+1)*(KMAX)*16
        qs_s = int((IMAXP+1),8)*(JMAXP+1)*(KMAX)*16
        speed_s = int((IMAXP+1),8)*(JMAXP+1)*(KMAX)*16
        rho_i_s = int((IMAXP+1),8)*(JMAXP+1)*(KMAX)*16
        square_s = int((IMAXP+1),8)*(JMAXP+1)*(KMAX)*16
        forcing_s = int((IMAXP+1),8)*(JMAXP+1)*(KMAX)*5*16
        u_s = int((IMAXP+1),8)*(JMAXP+1)*(KMAX)*5*16
        rhs_s = int((IMAXP+1),8)*(JMAXP+1)*(KMAX)*5*16
        usptr = numa_alloc(us_s,alloc_node)
        vsptr = numa_alloc(vs_s,alloc_node)
        wsptr = numa_alloc(ws_s,alloc_node)
        qsptr = numa_alloc(qs_s, alloc_node)
        rho_iptr = numa_alloc(rho_i_s,alloc_node)
        squareptr = numa_alloc(square_s,alloc_node)
        speedptr = numa_alloc(speed_s,alloc_node)
        forcingptr = numa_alloc(forcing_s,alloc_node)
        uptr = numa_alloc(u_s,alloc_node)
        rhsptr = numa_alloc(rhs_s,alloc_node)
        call c_f_pointer(usptr, us, [IMAXP+1,JMAXP+1,KMAX])
        call c_f_pointer(vsptr, vs, [IMAXP+1,JMAXP+1,KMAX])
        call c_f_pointer(wsptr, ws, [IMAXP+1,JMAXP+1,KMAX])
        call c_f_pointer(qsptr, qs, [IMAXP+1,JMAXP+1,KMAX])
        call c_f_pointer(rho_iptr, rho_i, [IMAXP+1,JMAXP+1,KMAX])
        call c_f_pointer(squareptr, square, [IMAXP+1,JMAXP+1,KMAX])
        call c_f_pointer(speedptr, speed, [IMAXP+1,JMAXP+1,KMAX])
        call c_f_pointer(forcingptr, forcing, [5,IMAXP+1,JMAXP+1,KMAX])
        call c_f_pointer(uptr, u, [5,IMAXP+1,JMAXP+1,KMAX])
        call c_f_pointer(rhsptr, rhs, [5,IMAXP+1,JMAXP+1,KMAX])

       do i = 1, t_last
          call timer_clear(i)
       end do

       call initialize(u)

       call exact_rhs(forcing)

c---------------------------------------------------------------------
c      do one time step to touch all code, and reinitialize
c---------------------------------------------------------------------
       call adi(rhs,qs,square,speed,forcing,ws,u,
     >   vs,us,rho_i,IMAXP,JMAXP,KMAX)
       call initialize(u)

       do i = 1, t_last
          call timer_clear(i)
       end do
       call timer_start(1)

       do  step = 1, niter

          if (mod(step, 20) .eq. 0 .or. step .eq. 1) then
             write(*, 200) step
 200         format(' Time step ', i4)
          endif

          call adi(rhs,qs,square,forcing,ws,u,vs,us,rho_i,
     > speed,IMAXP,JMAXP,KMAX)

       end do

       call timer_stop(1)
       tmax = timer_read(1)
       
       call verify(no_time_steps, class, verified,us,vs,ws,qs,
     >  rho_i,square,speed,forcing,u,rhs)

       if( tmax .ne. 0. ) then
          n3 = grid_points(1)*grid_points(2)*grid_points(3)
          t = (grid_points(1)+grid_points(2)+grid_points(3))/3.0
          mflops = (881.174 * float( n3 )
     >             -4683.91 * t**2
     >             +11484.5 * t
     >             -19272.4) * float( niter ) / (tmax*1000000.0d0)
       else
          mflops = 0.0
       endif

      call print_results('SP', class, grid_points(1), 
     >     grid_points(2), grid_points(3), niter, 
     >     tmax, mflops, '          floating point', 
     >     verified, npbversion,compiletime, cs1, cs2, cs3, cs4, cs5, 
     >     cs6, '(none)')

c---------------------------------------------------------------------
c      More timers
c---------------------------------------------------------------------
       if (.not.timeron) goto 999

       do i=1, t_last
          trecs(i) = timer_read(i)
       end do
       if (tmax .eq. 0.0) tmax = 1.0

       write(*,800)
 800   format('  SECTION   Time (secs)')

       do i=1, t_last
          write(*,810) t_names(i), trecs(i), trecs(i)*100./tmax
          if (i.eq.t_rhs) then
             t = trecs(t_rhsx) + trecs(t_rhsy) + trecs(t_rhsz)
             write(*,820) 'sub-rhs', t, t*100./tmax
             t = trecs(t_rhs) - t
             write(*,820) 'rest-rhs', t, t*100./tmax
          elseif (i.eq.t_zsolve) then
             t = trecs(t_zsolve) - trecs(t_rdis1) - trecs(t_rdis2)
             write(*,820) 'sub-zsol', t, t*100./tmax
          elseif (i.eq.t_rdis2) then
             t = trecs(t_rdis1) + trecs(t_rdis2)
             write(*,820) 'redist', t, t*100./tmax
          endif
 810      format(2x,a8,':',f9.3,'  (',f6.2,'%)')
 820      format('    --> ',a8,':',f9.3,'  (',f6.2,'%)')
       end do

 999   continue

       end
