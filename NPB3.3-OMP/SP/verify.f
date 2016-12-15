
c---------------------------------------------------------------------
c---------------------------------------------------------------------

        subroutine verify(no_time_steps,class,verified,us,vs,ws,qs,
     >  rho_i,square,speedforcing,u,rhs)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c  verification routine                         
c---------------------------------------------------------------------

        include 'header.h'

        double precision xcrref(5),xceref(5),xcrdif(5),xcedif(5), 
     >                   epsilon, xce(5), xcr(5), dtref
        integer m, no_time_steps
        character class
        logical verified
        double precision 
     >   us      (   0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   vs      (   0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   ws      (   0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   qs      (   0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   rho_i   (   0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   square  (   0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   speed   (   0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   forcing (5, 0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   u       (5, 0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   rhs     (5, 0:IMAXP, 0:JMAXP, 0:KMAX-1)

c---------------------------------------------------------------------
c   tolerance level
c---------------------------------------------------------------------
        epsilon = 1.0d-08


c---------------------------------------------------------------------
c   compute the error norm and the residual norm, and exit if not printing
c---------------------------------------------------------------------
        call error_norm(xce,u,rhs)
        call compute_rhs(rhs,qs,square,speed,forcing,ws,u,vs,us,rho_i)

        call rhs_norm(xcr,rhs)

        do m = 1, 5
           xcr(m) = xcr(m) / dt
        enddo

        class = 'U'
        verified = .true.

        do m = 1,5
           xcrref(m) = 1.0
           xceref(m) = 1.0
        end do

c---------------------------------------------------------------------
c    reference data for 12X12X12 grids after 100 time steps, with DT = 1.50d-02
c---------------------------------------------------------------------
        if ( (grid_points(1)  .eq. 12     ) .and. 
     >       (grid_points(2)  .eq. 12     ) .and.
     >       (grid_points(3)  .eq. 12     ) .and.
     >       (no_time_steps   .eq. 100    ))  then

           class = 'S'
           dtref = 1.5d-2

c---------------------------------------------------------------------
c    Reference values of RMS-norms of residual.
c---------------------------------------------------------------------
           xcrref(1) = 2.7470315451339479d-02
           xcrref(2) = 1.0360746705285417d-02
           xcrref(3) = 1.6235745065095532d-02
           xcrref(4) = 1.5840557224455615d-02
           xcrref(5) = 3.4849040609362460d-02

c---------------------------------------------------------------------
c    Reference values of RMS-norms of solution error.
c---------------------------------------------------------------------
           xceref(1) = 2.7289258557377227d-05
           xceref(2) = 1.0364446640837285d-05
           xceref(3) = 1.6154798287166471d-05
           xceref(4) = 1.5750704994480102d-05
           xceref(5) = 3.4177666183390531d-05


c---------------------------------------------------------------------
c    reference data for 36X36X36 grids after 400 time steps, with DT = 1.5d-03
c---------------------------------------------------------------------
        elseif ( (grid_points(1) .eq. 36) .and. 
     >           (grid_points(2) .eq. 36) .and.
     >           (grid_points(3) .eq. 36) .and.
     >           (no_time_steps . eq. 400) ) then

           class = 'W'
           dtref = 1.5d-3

c---------------------------------------------------------------------
c    Reference values of RMS-norms of residual.
c---------------------------------------------------------------------
           xcrref(1) = 0.1893253733584d-02
           xcrref(2) = 0.1717075447775d-03
           xcrref(3) = 0.2778153350936d-03
           xcrref(4) = 0.2887475409984d-03
           xcrref(5) = 0.3143611161242d-02

c---------------------------------------------------------------------
c    Reference values of RMS-norms of solution error.
c---------------------------------------------------------------------
           xceref(1) = 0.7542088599534d-04
           xceref(2) = 0.6512852253086d-05
           xceref(3) = 0.1049092285688d-04
           xceref(4) = 0.1128838671535d-04
           xceref(5) = 0.1212845639773d-03

c---------------------------------------------------------------------
c    reference data for 64X64X64 grids after 400 time steps, with DT = 1.5d-03
c---------------------------------------------------------------------
        elseif ( (grid_points(1) .eq. 64) .and. 
     >           (grid_points(2) .eq. 64) .and.
     >           (grid_points(3) .eq. 64) .and.
     >           (no_time_steps . eq. 400) ) then

           class = 'A'
           dtref = 1.5d-3

c---------------------------------------------------------------------
c    Reference values of RMS-norms of residual.
c---------------------------------------------------------------------
           xcrref(1) = 2.4799822399300195d0
           xcrref(2) = 1.1276337964368832d0
           xcrref(3) = 1.5028977888770491d0
           xcrref(4) = 1.4217816211695179d0
           xcrref(5) = 2.1292113035138280d0

c---------------------------------------------------------------------
c    Reference values of RMS-norms of solution error.
c---------------------------------------------------------------------
           xceref(1) = 1.0900140297820550d-04
           xceref(2) = 3.7343951769282091d-05
           xceref(3) = 5.0092785406541633d-05
           xceref(4) = 4.7671093939528255d-05
           xceref(5) = 1.3621613399213001d-04

c---------------------------------------------------------------------
c    reference data for 102X102X102 grids after 400 time steps,
c    with DT = 1.0d-03
c---------------------------------------------------------------------
        elseif ( (grid_points(1) .eq. 102) .and. 
     >           (grid_points(2) .eq. 102) .and.
     >           (grid_points(3) .eq. 102) .and.
     >           (no_time_steps . eq. 400) ) then

           class = 'B'
           dtref = 1.0d-3

c---------------------------------------------------------------------
c    Reference values of RMS-norms of residual.
c---------------------------------------------------------------------
           xcrref(1) = 0.6903293579998d+02
           xcrref(2) = 0.3095134488084d+02
           xcrref(3) = 0.4103336647017d+02
           xcrref(4) = 0.3864769009604d+02
           xcrref(5) = 0.5643482272596d+02

c---------------------------------------------------------------------
c    Reference values of RMS-norms of solution error.
c---------------------------------------------------------------------
           xceref(1) = 0.9810006190188d-02
           xceref(2) = 0.1022827905670d-02
           xceref(3) = 0.1720597911692d-02
           xceref(4) = 0.1694479428231d-02
           xceref(5) = 0.1847456263981d-01

c---------------------------------------------------------------------
c    reference data for 162X162X162 grids after 400 time steps,
c    with DT = 0.67d-03
c---------------------------------------------------------------------
        elseif ( (grid_points(1) .eq. 162) .and. 
     >           (grid_points(2) .eq. 162) .and.
     >           (grid_points(3) .eq. 162) .and.
     >           (no_time_steps . eq. 400) ) then

           class = 'C'
           dtref = 0.67d-3

c---------------------------------------------------------------------
c    Reference values of RMS-norms of residual.
c---------------------------------------------------------------------
           xcrref(1) = 0.5881691581829d+03
           xcrref(2) = 0.2454417603569d+03
           xcrref(3) = 0.3293829191851d+03
           xcrref(4) = 0.3081924971891d+03
           xcrref(5) = 0.4597223799176d+03

c---------------------------------------------------------------------
c    Reference values of RMS-norms of solution error.
c---------------------------------------------------------------------
           xceref(1) = 0.2598120500183d+00
           xceref(2) = 0.2590888922315d-01
           xceref(3) = 0.5132886416320d-01
           xceref(4) = 0.4806073419454d-01
           xceref(5) = 0.5483377491301d+00

c---------------------------------------------------------------------
c    reference data for 408X408X408 grids after 500 time steps,
c    with DT = 0.3d-03
c---------------------------------------------------------------------
        elseif ( (grid_points(1) .eq. 408) .and. 
     >           (grid_points(2) .eq. 408) .and.
     >           (grid_points(3) .eq. 408) .and.
     >           (no_time_steps . eq. 500) ) then

           class = 'D'
           dtref = 0.30d-3

c---------------------------------------------------------------------
c    Reference values of RMS-norms of residual.
c---------------------------------------------------------------------
           xcrref(1) = 0.1044696216887d+05
           xcrref(2) = 0.3204427762578d+04
           xcrref(3) = 0.4648680733032d+04
           xcrref(4) = 0.4238923283697d+04
           xcrref(5) = 0.7588412036136d+04

c---------------------------------------------------------------------
c    Reference values of RMS-norms of solution error.
c---------------------------------------------------------------------
           xceref(1) = 0.5089471423669d+01
           xceref(2) = 0.5323514855894d+00
           xceref(3) = 0.1187051008971d+01
           xceref(4) = 0.1083734951938d+01
           xceref(5) = 0.1164108338568d+02

c---------------------------------------------------------------------
c    reference data for 1020X1020X1020 grids after 500 time steps,
c    with DT = 0.1d-03
c---------------------------------------------------------------------
        elseif ( (grid_points(1) .eq. 1020) .and. 
     >           (grid_points(2) .eq. 1020) .and.
     >           (grid_points(3) .eq. 1020) .and.
     >           (no_time_steps . eq. 500) ) then

           class = 'E'
           dtref = 0.10d-3

c---------------------------------------------------------------------
c    Reference values of RMS-norms of residual.
c---------------------------------------------------------------------
           xcrref(1) = 0.6255387422609d+05
           xcrref(2) = 0.1495317020012d+05
           xcrref(3) = 0.2347595750586d+05
           xcrref(4) = 0.2091099783534d+05
           xcrref(5) = 0.4770412841218d+05

c---------------------------------------------------------------------
c    Reference values of RMS-norms of solution error.
c---------------------------------------------------------------------
           xceref(1) = 0.6742735164909d+02
           xceref(2) = 0.5390656036938d+01
           xceref(3) = 0.1680647196477d+02
           xceref(4) = 0.1536963126457d+02
           xceref(5) = 0.1575330146156d+03


        else
           verified = .false.
        endif

c---------------------------------------------------------------------
c    verification test for residuals if gridsize is one of 
c    the defined grid sizes above (class .ne. 'U')
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c    Compute the difference of solution values and the known reference values.
c---------------------------------------------------------------------
        do m = 1, 5
           
           xcrdif(m) = dabs((xcr(m)-xcrref(m))/xcrref(m)) 
           xcedif(m) = dabs((xce(m)-xceref(m))/xceref(m))
           
        enddo

c---------------------------------------------------------------------
c    Output the comparison of computed results to known cases.
c---------------------------------------------------------------------

        if (class .ne. 'U') then
           write(*, 1990) class
 1990      format(' Verification being performed for class ', a)
           write (*,2000) epsilon
 2000      format(' accuracy setting for epsilon = ', E20.13)
           verified = (dabs(dt-dtref) .le. epsilon)
           if (.not.verified) then  
              class = 'U'
              write (*,1000) dtref
 1000         format(' DT does not match the reference value of ', 
     >                 E15.8)
           endif
        else 
           write(*, 1995)
 1995      format(' Unknown class')
        endif


        if (class .ne. 'U') then
           write (*, 2001) 
        else
           write (*, 2005)
        endif

 2001   format(' Comparison of RMS-norms of residual')
 2005   format(' RMS-norms of residual')
        do m = 1, 5
           if (class .eq. 'U') then
              write(*, 2015) m, xcr(m)
           else if (xcrdif(m) .le. epsilon) then
              write (*,2011) m,xcr(m),xcrref(m),xcrdif(m)
           else 
              verified = .false.
              write (*,2010) m,xcr(m),xcrref(m),xcrdif(m)
           endif
        enddo

        if (class .ne. 'U') then
           write (*,2002)
        else
           write (*,2006)
        endif
 2002   format(' Comparison of RMS-norms of solution error')
 2006   format(' RMS-norms of solution error')
        
        do m = 1, 5
           if (class .eq. 'U') then
              write(*, 2015) m, xce(m)
           else if (xcedif(m) .le. epsilon) then
              write (*,2011) m,xce(m),xceref(m),xcedif(m)
           else
              verified = .false.
              write (*,2010) m,xce(m),xceref(m),xcedif(m)
           endif
        enddo
        
 2010   format(' FAILURE: ', i2, E20.13, E20.13, E20.13)
 2011   format('          ', i2, E20.13, E20.13, E20.13)
 2015   format('          ', i2, E20.13)
        
        if (class .eq. 'U') then
           write(*, 2022)
           write(*, 2023)
 2022      format(' No reference values provided')
 2023      format(' No verification performed')
        else if (verified) then
           write(*, 2020)
 2020      format(' Verification Successful')
        else
           write(*, 2021)
 2021      format(' Verification failed')
        endif

        return


        end
