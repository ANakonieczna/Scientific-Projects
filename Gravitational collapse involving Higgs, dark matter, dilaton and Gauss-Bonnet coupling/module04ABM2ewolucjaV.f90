MODULE moduleABM2ewolucjaV

CONTAINS
SUBROUTINE EwolucjaV(h0,N,pocz,&
 d,q1,q2,y,&
 !OUT:
 s1,s2,h,a,p1,p2,x,r,f,g,Q,beta)
!
USE modulepochodne
USE modulerhs
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: h0
INTEGER,INTENT(IN) :: N,pocz
REAL(8),DIMENSION(:),INTENT(IN) :: d,q1,q2,y
REAL(8),DIMENSION(:),INTENT(INOUT) :: s1,s2,h,a,p1,p2,x,r,f,g,Q,beta
!
REAL(8) :: invr
REAL(8) :: s1PRED,s2PRED,hPRED,aPRED,p1PRED,p2PRED,xPRED,rPRED,fPRED,gPRED,QPRED,betaPRED
INTEGER :: i

!******* calkowanie *******
!* metoda trapezow
i=pocz+1
a(i)=(a(i-1)+0.5D0*h0*a(i-1)*d(i-1))/(1.D0-0.5D0*h0*d(i))
s1(i)=s1(i-1)+0.5D0*h0*(q1(i-1)+q1(i))
s2(i)=s2(i-1)+0.5D0*h0*(q2(i-1)+q2(i))
h(i)=h(i-1)+0.5D0*h0*(y(i-1)+y(i))
!
g(i)=(g(i-1)+0.5D0*h0*(rhsg(h(i-1),q1(i-1),q2(i-1),y(i-1),d(i-1),r(i-1),g(i-1))&
 -(r(i-1)+0.5D0*h0*g(i-1))*gv1(h(i),q1(i),q2(i),y(i))))&
 /(1.D0-h0*d(i)+0.25D0*h0*h0*gv1(h(i),q1(i),q2(i),y(i)))
!
r(i)=r(i-1)+0.5D0*h0*(g(i-1)+g(i))
invr=1.D0/r(i)
!
Q(i)=(Q(i-1)+0.5D0*h0*(rhsQ(s1(i-1),s2(i-1),h(i-1),q1(i-1),q2(i-1),y(i-1),r(i-1),Q(i-1))+Qv1(s1(i),s2(i),h(i),q1(i),q2(i),r(i))))&
 /(1.D0+h0*alfa*y(i))
beta(i)=beta(i-1)+0.5D0*h0*(rhsbeta(a(i-1),r(i-1),Q(i-1))+rhsbeta(a(i),r(i),Q(i)))
!
f(i)=(f(i-1)+0.5D0*h0*(rhsgf(h(i-1),a(i-1),r(i-1),f(i-1),g(i-1),Q(i-1))+gfv1(h(i),a(i),r(i),Q(i))))/(1.D0+0.5D0*h0*g(i)*invr)
!
p1(i)=(p1(i-1)+0.5D0*h0*(rhsq1p1(s2(i-1),q1(i-1),q2(i-1),a(i-1),p1(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1))&
 +q1p1v1(s2(i),q1(i),q2(i),a(i),r(i),f(i),g(i),Q(i),beta(i))))/(1.D0+0.5D0*h0*g(i)*invr)
!
p2(i)=(p2(i-1)+0.5D0*h0*(rhsq2p2(s1(i-1),q1(i-1),q2(i-1),a(i-1),p2(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1))&
 +q2p2v1(s1(i),q1(i),q2(i),a(i),r(i),f(i),g(i),Q(i),beta(i))))/(1.D0+0.5D0*h0*g(i)*invr)
!
!* metoda Eulera
x(i)=x(i-1)&
 +h0*rhsyx(s1(i-1),s2(i-1),h(i-1),q1(i-1),q2(i-1),y(i-1),a(i-1),p1(i-1),p2(i-1),x(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1))

!* metoda Adams-Bashforth-Moulton 2
DO i=pocz+2,N
   aPRED=a(i-1)+0.5D0*h0*(3.D0*a(i-1)*d(i-1)-a(i-2)*d(i-2))
   s1PRED=s1(i-1)+0.5D0*h0*(3.D0*q1(i-1)-q1(i-2))
   s2PRED=s2(i-1)+0.5D0*h0*(3.D0*q2(i-1)-q2(i-2))
   hPRED=h(i-1)+0.5D0*h0*(3.D0*y(i-1)-y(i-2))
   !
   gPRED=g(i-1)+0.5D0*h0*(&
    3.D0*rhsg(h(i-1),q1(i-1),q2(i-1),y(i-1),d(i-1),r(i-1),g(i-1))-rhsg(h(i-2),q1(i-2),q2(i-2),y(i-2),d(i-2),r(i-2),g(i-2)))
   !
   rPRED=r(i-1)+0.5D0*h0*(3.D0*g(i-1)-g(i-2))
   !
   QPRED=Q(i-1)+0.5D0*h0*(3.D0*rhsQ(s1(i-1),s2(i-1),h(i-1),q1(i-1),q2(i-1),y(i-1),r(i-1),Q(i-1))&
    -rhsQ(s1(i-2),s2(i-2),h(i-2),q1(i-2),q2(i-2),y(i-2),r(i-2),Q(i-2)))
   betaPRED=beta(i-1)+0.5D0*h0*(3.D0*rhsbeta(a(i-1),r(i-1),Q(i-1))-rhsbeta(a(i-2),r(i-2),Q(i-2)))
   !
   fPRED=f(i-1)+0.5D0*h0*(3.D0*rhsgf(h(i-1),a(i-1),r(i-1),f(i-1),g(i-1),Q(i-1))-rhsgf(h(i-2),a(i-2),r(i-2),f(i-2),g(i-2),Q(i-2)))
   !
   p1PRED=p1(i-1)+0.5D0*h0*(3.D0*rhsq1p1(s2(i-1),q1(i-1),q2(i-1),a(i-1),p1(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1))&
    -rhsq1p1(s2(i-2),q1(i-2),q2(i-2),a(i-2),p1(i-2),r(i-2),f(i-2),g(i-2),Q(i-2),beta(i-2)))
   !
   p2PRED=p2(i-1)+0.5D0*h0*(3.D0*rhsq2p2(s1(i-1),q1(i-1),q2(i-1),a(i-1),p2(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1))&
    -rhsq2p2(s1(i-2),q1(i-2),q2(i-2),a(i-2),p2(i-2),r(i-2),f(i-2),g(i-2),Q(i-2),beta(i-2)))
   !
   xPRED=x(i-1)+0.5D0*h0*(&
    3.D0*rhsyx(s1(i-1),s2(i-1),h(i-1),q1(i-1),q2(i-1),y(i-1),a(i-1),p1(i-1),p2(i-1),x(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1))&
    -rhsyx(s1(i-2),s2(i-2),h(i-2),q1(i-2),q2(i-2),y(i-2),a(i-2),p1(i-2),p2(i-2),x(i-2),r(i-2),f(i-2),g(i-2),Q(i-2),beta(i-2)))
   !
   !*******
   !
   a(i)=a(i-1)+0.5D0*h0*(aPRED*d(i)+a(i-1)*d(i-1))
   s1(i)=s1(i-1)+0.5D0*h0*(q1(i)+q1(i-1))
   s2(i)=s2(i-1)+0.5D0*h0*(q2(i)+q2(i-1))
   h(i)=h(i-1)+0.5D0*h0*(y(i)+y(i-1))
   !
   g(i)=g(i-1)+0.5D0*h0*(rhsg(hPRED,q1(i),q2(i),y(i),d(i),rPRED,gPRED)+rhsg(h(i-1),q1(i-1),q2(i-1),y(i-1),d(i-1),r(i-1),g(i-1)))
   !
   r(i)=r(i-1)+0.5D0*h0*(gPRED+g(i-1))
   !
   Q(i)=Q(i-1)+0.5D0*h0*(rhsQ(s1PRED,s2PRED,hPRED,q1(i),q2(i),y(i),rPRED,QPRED)&
    +rhsQ(s1(i-1),s2(i-1),h(i-1),q1(i-1),q2(i-1),y(i-1),r(i-1),Q(i-1)))
   beta(i)=beta(i-1)+0.5D0*h0*(rhsbeta(aPRED,rPRED,QPRED)+rhsbeta(a(i-1),r(i-1),Q(i-1)))
   !
   f(i)=f(i-1)+0.5D0*h0*(rhsgf(hPRED,aPRED,rPRED,fPRED,gPRED,QPRED)+rhsgf(h(i-1),a(i-1),r(i-1),f(i-1),g(i-1),Q(i-1)))
   !
   p1(i)=p1(i-1)+0.5D0*h0*(rhsq1p1(s2PRED,q1(i),q2(i),aPRED,p1PRED,rPRED,fPRED,gPRED,QPRED,betaPRED)&
    +rhsq1p1(s2(i-1),q1(i-1),q2(i-1),a(i-1),p1(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1)))
   !
   p2(i)=p2(i-1)+0.5D0*h0*(rhsq2p2(s1PRED,q1(i),q2(i),aPRED,p2PRED,rPRED,fPRED,gPRED,QPRED,betaPRED)&
    +rhsq2p2(s1(i-1),q1(i-1),q2(i-1),a(i-1),p2(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1)))
   !
   x(i)=x(i-1)+0.5D0*h0*(rhsyx(s1PRED,s2PRED,hPRED,q1(i),q2(i),y(i),aPRED,p1PRED,p2PRED,xPRED,rPRED,fPRED,gPRED,QPRED,betaPRED)&
    +rhsyx(s1(i-1),s2(i-1),h(i-1),q1(i-1),q2(i-1),y(i-1),a(i-1),p1(i-1),p2(i-1),x(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1)))
END DO

END SUBROUTINE EwolucjaV

END MODULE moduleABM2ewolucjaV
