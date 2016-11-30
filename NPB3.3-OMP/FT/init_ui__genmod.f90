        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 29 22:08:41 2016
        MODULE INIT_UI__genmod
          INTERFACE 
            SUBROUTINE INIT_UI(U0,U1,TWIDDLE,D1,D2,D3)
              INTEGER(KIND=4) :: D3
              INTEGER(KIND=4) :: D2
              INTEGER(KIND=4) :: D1
              COMPLEX(KIND=8) :: U0(D1+1,D2,D3)
              COMPLEX(KIND=8) :: U1(D1+1,D2,D3)
              REAL(KIND=8) :: TWIDDLE(D1+1,D2,D3)
            END SUBROUTINE INIT_UI
          END INTERFACE 
        END MODULE INIT_UI__genmod
