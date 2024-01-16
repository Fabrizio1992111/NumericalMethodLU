        !COMPILER-GENERATED INTERFACE MODULE: Thu Aug 06 12:29:04 2020
        MODULE INVERSEM_D__genmod
          INTERFACE 
            FUNCTION INVERSEM_D(A,DET,IER) RESULT(AINV)
              REAL(KIND=8) :: A(:,:)
              REAL(KIND=8) ,OPTIONAL :: DET
              INTEGER(KIND=4) ,OPTIONAL :: IER
              REAL(KIND=8) :: AINV(SIZE(A,1),SIZE(A,1))
            END FUNCTION INVERSEM_D
          END INTERFACE 
        END MODULE INVERSEM_D__genmod
