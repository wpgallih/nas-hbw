        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 29 22:08:41 2016
        MODULE CFFTS2__genmod
          INTERFACE 
            SUBROUTINE CFFTS2(U,IS,D1,D2,D3,X,XOUT,Y1,Y2)
              COMMON/BLOCKINFO/ FFTBLOCK,FFTBLOCKPAD
                INTEGER(KIND=4) :: FFTBLOCK
                INTEGER(KIND=4) :: FFTBLOCKPAD
              INTEGER(KIND=4) :: D3
              INTEGER(KIND=4) :: D2
              INTEGER(KIND=4) :: D1
              COMPLEX(KIND=8) :: U(*)
              INTEGER(KIND=4) :: IS
              COMPLEX(KIND=8) :: X(D1+1,D2,D3)
              COMPLEX(KIND=8) :: XOUT(D1+1,D2,D3)
              COMPLEX(KIND=8) :: Y1(FFTBLOCKPAD,D2)
              COMPLEX(KIND=8) :: Y2(FFTBLOCKPAD,D2)
            END SUBROUTINE CFFTS2
          END INTERFACE 
        END MODULE CFFTS2__genmod
