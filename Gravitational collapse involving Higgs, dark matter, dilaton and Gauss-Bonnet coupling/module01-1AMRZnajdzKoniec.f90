MODULE moduleZnajdzKoniecAMR

CONTAINS
SUBROUTINE ZnajdzKoniecAMR(NAMRN,bladAMR,rAMRNp,rAMRNn,poczAMR,&
 !OUT:
 konAMR)
IMPLICIT NONE
INTEGER,INTENT(IN) :: NAMRN,poczAMR
REAL(8),INTENT(IN) :: bladAMR
REAL(8),DIMENSION(:),INTENT(IN) :: rAMRNp,rAMRNn
INTEGER,INTENT(OUT) :: konAMR
!
REAL(8) :: funkcjableduAMR
INTEGER :: i

DO i=poczAMR+1,NAMRN
   funkcjableduAMR=ABS((rAMRNn(i)-rAMRNp(i))/rAMRNp(i))
   IF (funkcjableduAMR.LT.bladAMR) THEN
      konAMR=i
      EXIT
    ELSE
      konAMR=NAMRN
   END IF
END DO

END SUBROUTINE ZnajdzKoniecAMR

END MODULE moduleZnajdzKoniecAMR
