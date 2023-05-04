MODULE moduleRK2ewolucjaU

CONTAINS
SUBROUTINE EwolucjaU(h0V,h0U,N,pocz,czyAMR,licznik,&
 dprev,q1prev,q2prev,yprev,s1prev,s2prev,hprev,aprev,p1prev,p2prev,xprev,rprev,fprev,gprev,Qprev,betaprev,&
 s1prev2,s2prev2,hprev2,aprev2,p1prev2,p2prev2,xprev2,fprev2,Qprev2,betaprev2,&
 s1next,s2next,hnext,anext,p1next,p2next,xnext,fnext,Qnext,betanext,&
 !OUT:
 dnext,q1next,q2next,ynext)
!
USE moduleRK2du
IMPLICIT NONE
REAL(8),INTENT(IN) :: h0V,h0U
INTEGER,INTENT(IN) :: N,pocz
LOGICAL,INTENT(IN) :: czyAMR
INTEGER,INTENT(IN) :: licznik
REAL(8),DIMENSION(:),INTENT(IN) :: dprev,q1prev,q2prev,yprev,s1prev,s2prev,hprev,aprev,p1prev,p2prev,xprev,rprev
REAL(8),DIMENSION(:),INTENT(IN) :: fprev,gprev,Qprev,betaprev
REAL(8),INTENT(IN) :: s1prev2,s2prev2,hprev2,aprev2,p1prev2,p2prev2,xprev2,fprev2,Qprev2,betaprev2
REAL(8),INTENT(IN) :: s1next,s2next,hnext,anext,p1next,p2next,xnext,fnext,Qnext,betanext
REAL(8),DIMENSION(:),INTENT(OUT) :: dnext,q1next,q2next,ynext
!
REAL(8),DIMENSION(N) :: k2d,k2q1,k2q2,k2y
INTEGER :: i

CALL RK2du(h0V,h0U,N,pocz,czyAMR,licznik,&
 dprev,q1prev,q2prev,yprev,s1prev,s2prev,hprev,aprev,p1prev,p2prev,xprev,rprev,fprev,gprev,Qprev,betaprev,&
 s1prev2,s2prev2,hprev2,aprev2,p1prev2,p2prev2,xprev2,fprev2,Qprev2,betaprev2,&
 s1next,s2next,hnext,anext,p1next,p2next,xnext,fnext,Qnext,betanext,&
 !OUT:
 k2d,k2q1,k2q2,k2y)

DO i=pocz,N
   dnext(i)=dprev(i)+h0U*k2d(i)
   q1next(i)=q1prev(i)+h0U*k2q1(i)
   q2next(i)=q2prev(i)+h0U*k2q2(i)
   ynext(i)=yprev(i)+h0U*k2y(i)
END DO

END SUBROUTINE EwolucjaU

END MODULE moduleRK2ewolucjaU
