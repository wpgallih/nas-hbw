
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine  adi(rhs,qs,square,forcing,ws,u,vs,us,
     >   rho_i,IMAXP,JMAXP,KMAX)

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
     >   forcing (5, 0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   u       (5, 0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   rhs     (5, 0:IMAXP, 0:JMAXP, 0:KMAX-1)
       call compute_rhs(rhs,qs,square,forcing,ws,u,vs,us,rho_i)

       call txinvr(rhs,qs,square,forcing,ws,u,vs,us,rho_i)

       call x_solve(rho_i, u, qs, square, rhs)

       call y_solve(rho_i, u, qs, square, rhs)

       call z_solve(u, qs, square, rhs, rho_i)

       call add(u, rhs)

       return
       end

