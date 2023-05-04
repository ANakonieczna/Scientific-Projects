MODULE moduleWarunkiPoczatkowe

CONTAINS
SUBROUTINE WarunkiPoczatkowe(h0,N,Nkoniec,v,&
 !OUT:
 d,q1,q2,y,s1,s2,h,a,p1,p2,x,r,f,g,Q,beta)
 
USE modulerhs
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
REAL(8) :: ps,ph,v0h,Dh,vi,vf,pi,phdiff
REAL(8) :: r0ini
COMMON /PARAMSmodel/ alfa,gamma,e
COMMON /PARAMSprofile/ ps,ph,v0h,Dh,vi,vf,pi,phdiff
COMMON /PARAMSini/ r0ini
!*************************
REAL(8),INTENT(IN) :: h0
INTEGER,INTENT(IN) :: N,Nkoniec
REAL(8),DIMENSION(:),INTENT(IN) :: v
REAL(8),DIMENSION(:),INTENT(INOUT) :: d,q1,q2,y,s1,s2,h,a,p1,p2,x,r,f,g,Q,beta
!
REAL(8) :: invr
REAL(8) :: aPRED,QPRED,betaPRED,fPRED,p1PRED,p2PRED,xPRED
INTEGER :: i

g=0.5D0

!******* profile poczatkowe *******
!--- 1 ---
!s1=ps*DSIN(pi*v/(Nkoniec*h0))*DSIN(pi*v/(Nkoniec*h0))*DCOS(2.D0*pi*v/(Nkoniec*h0))
!s2=ps*DSIN(pi*v/(Nkoniec*h0))*DSIN(pi*v/(Nkoniec*h0))*DCOS(2.D0*pi*v/(Nkoniec*h0)+phdiff)
!h=ph*v*v*DEXP(-(v-v0h)*(v-v0h)/(Dh*Dh))
!q1=ps*pi/(Nkoniec*h0)*DSIN(2.D0*pi*v/(Nkoniec*h0))*(2.D0*DCOS(2.D0*pi*v/(Nkoniec*h0))-1.D0)
!q2=2.D0*ps*pi/(Nkoniec*h0)*DSIN(pi*v/(Nkoniec*h0))*DCOS(3.D0*pi*v/(Nkoniec*h0)+phdiff)
!y=2.D0*ph*v/(Dh*Dh)*DEXP(-(v-v0h)*(v-v0h)/(Dh*Dh))*(Dh*Dh-v*(v-v0h))
!--- 2 ---
s1=ps*DCOS(2.D0*pi*(v-vi)/(vf-vi))&
 *DSIN(pi*(v-vi)/(vf-vi))*DSIN(pi*(v-vi)/(vf-vi))*DSIN(pi*(v-vi)/(vf-vi))*DSIN(pi*(v-vi)/(vf-vi))
s2=ps*DCOS(phdiff+2.D0*pi*(v-vi)/(vf-vi))&
 *DSIN(pi*(v-vi)/(vf-vi))*DSIN(pi*(v-vi)/(vf-vi))*DSIN(pi*(v-vi)/(vf-vi))*DSIN(pi*(v-vi)/(vf-vi))
h=ph*v*v*(v-vf)*(v-vf)*(v-vf)*DEXP(-(v-v0h)*(v-v0h)/(Dh*Dh))
q1=pi*ps/(vf-vi)*(DCOS(pi*(v-vi)/(vf-vi))+3.D0*DCOS(3.D0*pi*(v-vi)/(vf-vi)))&
 *DSIN(pi*(v-vi)/(vf-vi))*DSIN(pi*(v-vi)/(vf-vi))*DSIN(pi*(v-vi)/(vf-vi))
q2=pi*ps/(vf-vi)*(DCOS(phdiff+pi*(v-vi)/(vf-vi))+3.D0*DCOS(phdiff+3.D0*pi*(v-vi)/(vf-vi)))&
 *DSIN(pi*(v-vi)/(vf-vi))*DSIN(pi*(v-vi)/(vf-vi))*DSIN(pi*(v-vi)/(vf-vi))
y=ph*v*(v-vf)*(v-vf)/(Dh*Dh)*DEXP(-(v-v0h)*(v-v0h)/(Dh*Dh))*(Dh*Dh*(5.D0*v-2.D0*vf)-2.D0*v*(v-v0h)*(v-vf))
s1(25001:N)=0.D0
s2(25001:N)=0.D0
h(25001:N)=0.D0
q1(25001:N)=0.D0
q2(25001:N)=0.D0
y(25001:N)=0.D0

!*** W PKCIE (0,0) ***
r(1)=r0ini
a(1)=1.D0
Q(1)=0.D0
beta(1)=0.D0
f(1)=-0.5D0
p1(1)=0.D0
p2(1)=0.D0
x(1)=0.D0

!******* calkowanie *******
!* metoda trapezow; Adams-Bashforth-Moulton 2
DO i=2,N
   r(i)=r(i-1)+0.5D0*h0*(g(i-1)+g(i))
END DO

!*** z gPOv ***
d=0.5D0*r/g*(y*y+0.25D0*DEXP(2.D0*h*(alfa+1.D0))*(q1*q1+q2*q2))

!******* calkowanie *******
!* metoda trapezow
i=2
invr=1.D0/r(i)
!
a(i)=(1.D0+0.5D0*h0*d(i-1))/(1.D0-0.5D0*h0*d(i))
!
Q(i)=0.5D0*h0*(rhsQ(s1(i-1),s2(i-1),h(i-1),q1(i-1),q2(i-1),y(i-1),r(i-1),Q(i-1))+Qv1(s1(i),s2(i),h(i),q1(i),q2(i),r(i)))&
 /(1.D0+h0*alfa*y(i))
beta(i)=0.5D0*h0*(rhsbeta(a(i-1),r(i-1),Q(i-1))+rhsbeta(a(i),r(i),Q(i)))
!
f(i)=(f(i-1)+0.5D0*h0*(rhsgf(h(i-1),a(i-1),r(i-1),f(i-1),g(i-1),Q(i-1))+gfv1(h(i),a(i),r(i),Q(i))))/(1.D0+0.5D0*h0*g(i)*invr)
!
p1(i)=0.5D0*h0*(rhsq1p1(s2(i-1),q1(i-1),q2(i-1),a(i-1),p1(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1))&
 +q1p1v1(s2(i),q1(i),q2(i),a(i),r(i),f(i),g(i),Q(i),beta(i)))/(1.D0+0.5D0*h0*g(i)*invr)
!
p2(i)=0.5D0*h0*(rhsq2p2(s1(i-1),q1(i-1),q2(i-1),a(i-1),p2(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1))&
 +q2p2v1(s1(i),q1(i),q2(i),a(i),r(i),f(i),g(i),Q(i),beta(i)))/(1.D0+0.5D0*h0*g(i)*invr)
!
!* metoda Eulera
x(i)=h0*rhsyx(s1(i-1),s2(i-1),h(i-1),q1(i-1),q2(i-1),y(i-1),a(i-1),p1(i-1),p2(i-1),x(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1))

!* metoda Adams-Bashforth-Moulton 2
DO i=3,N
   aPRED=a(i-1)+0.5D0*h0*(3.D0*a(i-1)*d(i-1)-a(i-2)*d(i-2))
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
   !
   Q(i)=Q(i-1)+0.5D0*h0*(rhsQ(s1(i),s2(i),h(i),q1(i),q2(i),y(i),r(i),QPRED)&
    +rhsQ(s1(i-1),s2(i-1),h(i-1),q1(i-1),q2(i-1),y(i-1),r(i-1),Q(i-1)))
   beta(i)=beta(i-1)+0.5D0*h0*(rhsbeta(aPRED,r(i),QPRED)+rhsbeta(a(i-1),r(i-1),Q(i-1)))
   !
   f(i)=f(i-1)+0.5D0*h0*(rhsgf(h(i),aPRED,r(i),fPRED,g(i),QPRED)&
    +rhsgf(h(i-1),a(i-1),r(i-1),f(i-1),g(i-1),Q(i-1)))
   !
   p1(i)=p1(i-1)+0.5D0*h0*(rhsq1p1(s2(i),q1(i),q2(i),aPRED,p1PRED,r(i),fPRED,g(i),QPRED,betaPRED)&
    +rhsq1p1(s2(i-1),q1(i-1),q2(i-1),a(i-1),p1(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1)))
   !
   p2(i)=p2(i-1)+0.5D0*h0*(rhsq2p2(s1(i),q1(i),q2(i),aPRED,p2PRED,r(i),fPRED,g(i),QPRED,betaPRED)&
    +rhsq2p2(s1(i-1),q1(i-1),q2(i-1),a(i-1),p2(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1)))
   !
   x(i)=x(i-1)+0.5D0*h0*(rhsyx(s1(i),s2(i),h(i),q1(i),q2(i),y(i),aPRED,p1PRED,p2PRED,xPRED,r(i),fPRED,g(i),QPRED,betaPRED)&
    +rhsyx(s1(i-1),s2(i-1),h(i-1),q1(i-1),q2(i-1),y(i-1),a(i-1),p1(i-1),p2(i-1),x(i-1),r(i-1),f(i-1),g(i-1),Q(i-1),beta(i-1)))
END DO

END SUBROUTINE WarunkiPoczatkowe

END MODULE moduleWarunkiPoczatkowe
