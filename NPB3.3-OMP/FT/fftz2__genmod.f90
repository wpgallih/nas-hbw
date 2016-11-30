        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 29 22:08:41 2016
        MODULE FFTZ2__genmod
          INTERFACE 
            SUBROUTINE FFTZ2(IS,L,M,N,NY,NY1,U,X,Y)
              INTEGER(KIND=4) :: NY1
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: IS
              INTEGER(KIND=4) :: L
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: NY
              COMPLEX(KIND=8) :: U(N)
              COMPLEX(KIND=8) :: X(NY1,N)
              COMPLEX(KIND=8) :: Y(NY1,N)
            END SUBROUTINE FFTZ2
          END INTERFACE 
        END MODULE FFTZ2__genmod
