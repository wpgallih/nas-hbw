
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine  adi(rhs,qs,square,forcing,ws,u,vs,us,
     >   rho_i,speed,IMAXP,JMAXP,KMAX)

c---------------------------------------------------------------------
c---------------------------------------------------------------------
		integer IMAXP, JMAXP, KMAX
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
       call compute_rhs(rhs,qs,square,speed,forcing,ws,u,vs,us,rho_i)

       call txinvr(rhs,qs,square,speed,forcing,ws,u,vs,us,rho_i)

       call x_solve(rho_i, u, qs, us, square, speed, rhs)

       call y_solve(rho_i, u, qs, square, rhs, vs, speed)

       call z_solve(u, qs, square, rhs, rho_i,forcing,ws,vs,us,speed)

       call add(u, rhs)

       return
       end

