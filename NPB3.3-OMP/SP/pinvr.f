
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine pinvr(rhs)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   block-diagonal matrix-vector multiplication                       
c---------------------------------------------------------------------

       include 'header.h'

       integer i, j, k
       double precision r1, r2, r3, r4, r5, t1, t2

        double precision 
     >   rhs     (5, 0:IMAXP, 0:JMAXP, 0:KMAX-1)

       if (timeron) call timer_start(t_pinvr)
!$omp parallel do default(shared) private(i,j,k,r1,r2,r3,r4,r5,t1,t2)
       do   k = 1, nz2
          do   j = 1, ny2
             do   i = 1, nx2

                r1 = rhs(1,i,j,k)
                r2 = rhs(2,i,j,k)
                r3 = rhs(3,i,j,k)
                r4 = rhs(4,i,j,k)
                r5 = rhs(5,i,j,k)

                t1 = bt * r1
                t2 = 0.5d0 * ( r4 + r5 )

                rhs(1,i,j,k) =  bt * ( r4 - r5 )
                rhs(2,i,j,k) = -r3
                rhs(3,i,j,k) =  r2
                rhs(4,i,j,k) = -t1 + t2
                rhs(5,i,j,k) =  t1 + t2
             end do
          end do
       end do
       if (timeron) call timer_stop(t_pinvr)

       return
       end



