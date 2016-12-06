c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine  initialize(u)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     This subroutine initializes the field variable u using 
c     tri-linear transfinite interpolation of the boundary values     
c---------------------------------------------------------------------

      include 'header.h'
      
      integer i, j, k, m, ix, iy, iz
      double precision  xi, eta, zeta, Pface(5,3,2), Pxi, Peta, 
     >     Pzeta, temp(5)
        double precision u(5,0:IMAXP,0:JMAXP,0:JMAX-1)


!$omp parallel default(shared)
!$omp& private(i,j,k,m,zeta,eta,xi,ix,iy,iz,Pface,Pxi,Peta,Pzeta,temp)
c---------------------------------------------------------------------
c  Later (in compute_rhs) we compute 1/u for every element. A few of 
c  the corner elements are not used, but it convenient (and faster) 
c  to compute the whole thing with a simple loop. Make sure those 
c  values are nonzero by initializing the whole thing here. 
c---------------------------------------------------------------------
!$omp do schedule(static)
      do k = 0, grid_points(3)-1
         do j = 0, grid_points(2)-1
            do i = 0, grid_points(1)-1
               do m = 1, 5
                  u(m,i,j,k) = 1.0
               end do
            end do
         end do
      end do
!$omp end do
c---------------------------------------------------------------------


c---------------------------------------------------------------------
c     first store the "interpolated" values everywhere on the grid    
c---------------------------------------------------------------------

!$omp do schedule(static)
      do k = 0, grid_points(3)-1
         zeta = dble(k) * dnzm1
         do j = 0, grid_points(2)-1
            eta = dble(j) * dnym1
            do i = 0, grid_points(1)-1
               xi = dble(i) * dnxm1
                  
               do ix = 1, 2
                  call exact_solution(dble(ix-1), eta, zeta, 
     >                    Pface(1,1,ix))
               enddo

               do iy = 1, 2
                  call exact_solution(xi, dble(iy-1) , zeta, 
     >                    Pface(1,2,iy))
               enddo

               do iz = 1, 2
                  call exact_solution(xi, eta, dble(iz-1),   
     >                    Pface(1,3,iz))
               enddo

               do m = 1, 5
                  Pxi   = xi   * Pface(m,1,2) + 
     >                    (1.0d0-xi)   * Pface(m,1,1)
                  Peta  = eta  * Pface(m,2,2) + 
     >                    (1.0d0-eta)  * Pface(m,2,1)
                  Pzeta = zeta * Pface(m,3,2) + 
     >                    (1.0d0-zeta) * Pface(m,3,1)
                     
                  u(m,i,j,k) = Pxi + Peta + Pzeta - 
     >                    Pxi*Peta - Pxi*Pzeta - Peta*Pzeta + 
     >                    Pxi*Peta*Pzeta

               enddo
            enddo
         enddo
      enddo
!$omp end do nowait

c---------------------------------------------------------------------
c     now store the exact values on the boundaries        
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     west face                                                  
c---------------------------------------------------------------------
      i = 0
      xi = 0.0d0
!$omp do schedule(static)
      do k = 0, grid_points(3)-1
         zeta = dble(k) * dnzm1
         do j = 0, grid_points(2)-1
            eta = dble(j) * dnym1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do nowait

c---------------------------------------------------------------------
c     east face                                                      
c---------------------------------------------------------------------

      i = grid_points(1)-1
      xi = 1.0d0
!$omp do schedule(static)
      do k = 0, grid_points(3)-1
         zeta = dble(k) * dnzm1
         do j = 0, grid_points(2)-1
            eta = dble(j) * dnym1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do nowait

c---------------------------------------------------------------------
c     south face                                                 
c---------------------------------------------------------------------
      j = 0
      eta = 0.0d0
!$omp do schedule(static)
      do k = 0, grid_points(3)-1
         zeta = dble(k) * dnzm1
         do i = 0, grid_points(1)-1
            xi = dble(i) * dnxm1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do nowait


c---------------------------------------------------------------------
c     north face                                    
c---------------------------------------------------------------------
      j = grid_points(2)-1
      eta = 1.0d0
!$omp do schedule(static)
      do k = 0, grid_points(3)-1
         zeta = dble(k) * dnzm1
         do i = 0, grid_points(1)-1
            xi = dble(i) * dnxm1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do

c---------------------------------------------------------------------
c     bottom face                                       
c---------------------------------------------------------------------
      k = 0
      zeta = 0.0d0
!$omp do schedule(static)
      do j = 0, grid_points(2)-1
         eta = dble(j) * dnym1
         do i =0, grid_points(1)-1
            xi = dble(i) *dnxm1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do nowait

c---------------------------------------------------------------------
c     top face     
c---------------------------------------------------------------------
      k = grid_points(3)-1
      zeta = 1.0d0
!$omp do schedule(static)
      do j = 0, grid_points(2)-1
         eta = dble(j) * dnym1
         do i =0, grid_points(1)-1
            xi = dble(i) * dnxm1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do nowait
!$omp end parallel

      return
      end


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine lhsinit(lhs, ni)

c---------------------------------------------------------------------
c---------------------------------------------------------------------
      
      integer i, m, n, ni
      double precision lhs(5,5,3,0:ni)

c---------------------------------------------------------------------
c     zero the whole left hand side for starters
c     set all diagonal values to 1. This is overkill, but convenient
c---------------------------------------------------------------------
      i = 0
      do m = 1, 5
         do n = 1, 5
            lhs(m,n,1,i) = 0.0d0
            lhs(m,n,2,i) = 0.0d0
            lhs(m,n,3,i) = 0.0d0
         end do
         lhs(m,m,2,i) = 1.0d0
      end do
      i = ni
      do m = 1, 5
         do n = 1, 5
            lhs(m,n,1,i) = 0.0d0
            lhs(m,n,2,i) = 0.0d0
            lhs(m,n,3,i) = 0.0d0
         end do
         lhs(m,m,2,i) = 1.0d0
      end do

      return
      end



