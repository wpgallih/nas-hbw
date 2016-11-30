        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 29 22:08:41 2016
        MODULE CFFTZ__genmod
          INTERFACE 
            SUBROUTINE CFFTZ(U,IS,M,N,X,Y)
              COMMON/BLOCKINFO/ FFTBLOCK,FFTBLOCKPAD
                INTEGER(KIND=4) :: FFTBLOCK
                INTEGER(KIND=4) :: FFTBLOCKPAD
              INTEGER(KIND=4) :: N
              COMPLEX(KIND=8) :: U(*)
              INTEGER(KIND=4) :: IS
              INTEGER(KIND=4) :: M
              COMPLEX(KIND=8) :: X(FFTBLOCKPAD,N)
              COMPLEX(KIND=8) :: Y(FFTBLOCKPAD,N)
            END SUBROUTINE CFFTZ
          END INTERFACE 
        END MODULE CFFTZ__genmod
