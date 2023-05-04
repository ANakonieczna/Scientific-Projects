MODULE moduleTworzenieTablicyFUNpppn

CONTAINS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!!! PROCEDURA zwracajaca macierz wartosci funkcji interpolowanej wzdluz v(poczatekAMR) --> vAMR(1) !!!!!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE TworzenieTablicyFUNpppn(FUNpp,FUNp,FUNn,&
 !OUT:
 FUNpppn)
IMPLICIT NONE
REAL(8),INTENT(IN) :: FUNpp,FUNp,FUNn
REAL(8),INTENT(OUT) :: FUNpppn(3)

FUNpppn(1)=FUNpp
FUNpppn(2)=FUNp
FUNpppn(3)=FUNn

END SUBROUTINE TworzenieTablicyFUNpppn

END MODULE moduleTworzenieTablicyFUNpppn
