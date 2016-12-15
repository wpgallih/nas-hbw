
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine  txinvr(rhs,qs,square,speed,
     >    forcing,ws,u,vs,us,rho_i)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c block-diagonal matrix-vector multiplication                  
c---------------------------------------------------------------------

       include 'header.h'

       integer i, j, k
       double precision t1, t2, t3, ac, ru1, uu, vv, ww, r1, r2, r3, 
     >                  r4, r5, ac2inv
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

       if (timeron) call timer_start(t_txinvr)
!$omp parallel do default(shared)
!$omp& private(i,j,k,t1,t2,t3,ac,ru1,uu,vv,ww,r1,r2,r3,r4,r5,ac2inv)
       do    k = 1, nz2
          do    j = 1, ny2
             do    i = 1, nx2

                ru1 = rho_i(i,j,k)
                uu = us(i,j,k)
                vv = vs(i,j,k)
                ww = ws(i,j,k)
                ac = speed(i,j,k)
                ac2inv = ac*ac

                r1 = rhs(1,i,j,k)
                r2 = rhs(2,i,j,k)
                r3 = rhs(3,i,j,k)
                r4 = rhs(4,i,j,k)
                r5 = rhs(5,i,j,k)

                t1 = c2 / ac2inv * ( qs(i,j,k)*r1 - uu*r2  - 
     >                  vv*r3 - ww*r4 + r5 )
                t2 = bt * ru1 * ( uu * r1 - r2 )
                t3 = ( bt * ru1 * ac ) * t1

                rhs(1,i,j,k) = r1 - t1
                rhs(2,i,j,k) = - ru1 * ( ww*r1 - r4 )
                rhs(3,i,j,k) =   ru1 * ( vv*r1 - r3 )
                rhs(4,i,j,k) = - t2 + t3
                rhs(5,i,j,k) =   t2 + t3

             end do
          end do
       end do
       if (timeron) call timer_stop(t_txinvr)

       return
       end


