
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine  add(u, rhs)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c addition of update to the vector u
c---------------------------------------------------------------------

       include 'header.h'

       integer i,j,k,m
       double precision 
     >   u       (5, 0:IMAXP, 0:JMAXP, 0:KMAX-1),
     >   rhs     (5, 0:IMAXP, 0:JMAXP, 0:KMAX-1)

       if (timeron) call timer_start(t_add)
!$omp parallel do default(shared) private(i,j,k,m)
       do k = 1, nz2
          do j = 1, ny2
             do i = 1, nx2
                do m = 1, 5
                   u(m,i,j,k) = u(m,i,j,k) + rhs(m,i,j,k)
                end do
             end do
          end do
       end do
       if (timeron) call timer_stop(t_add)

       return
       end

