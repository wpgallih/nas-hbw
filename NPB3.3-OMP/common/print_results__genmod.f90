        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 29 22:08:42 2016
        MODULE PRINT_RESULTS__genmod
          INTERFACE 
            SUBROUTINE PRINT_RESULTS(NAME,CLASS,N1,N2,N3,NITER,T,MOPS,  &
     &OPTYPE,VERIFIED,NPBVERSION,COMPILETIME,CS1,CS2,CS3,CS4,CS5,CS6,CS7&
     &)
              CHARACTER(*) :: NAME
              CHARACTER(LEN=1) :: CLASS
              INTEGER(KIND=4) :: N1
              INTEGER(KIND=4) :: N2
              INTEGER(KIND=4) :: N3
              INTEGER(KIND=4) :: NITER
              REAL(KIND=8) :: T
              REAL(KIND=8) :: MOPS
              CHARACTER(LEN=24) :: OPTYPE
              LOGICAL(KIND=4) :: VERIFIED
              CHARACTER(*) :: NPBVERSION
              CHARACTER(*) :: COMPILETIME
              CHARACTER(*) :: CS1
              CHARACTER(*) :: CS2
              CHARACTER(*) :: CS3
              CHARACTER(*) :: CS4
              CHARACTER(*) :: CS5
              CHARACTER(*) :: CS6
              CHARACTER(*) :: CS7
            END SUBROUTINE PRINT_RESULTS
          END INTERFACE 
        END MODULE PRINT_RESULTS__genmod
