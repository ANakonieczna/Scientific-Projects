MODULE modulerhs

CONTAINS

!*******************************************************************************************************************
REAL(8) FUNCTION rhsd(s1,s2,h,q1,q2,y,a,p1,p2,x,r,f,g,Q,beta)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s1,s2,h,q1,q2,y,a,p1,p2,x,r,f,g,Q,beta
REAL(8) :: invr

invr=1.D0/r

rhsd=(0.25D0*a*a+f*g)*invr*invr-x*y-0.25D0*DEXP(2.D0*h*(alfa+1.D0))*(p1*q1+p2*q2+e*beta*(s1*q2-s2*q1))&
 -0.5D0*DEXP(2.D0*alfa*h)*Q*Q*a*a*invr*invr*invr*invr

END FUNCTION rhsd

!*******************************************************************************************************************
REAL(8) FUNCTION rhsq1p1(s2,q1,q2,a,p1,r,f,g,Q,beta)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s2,q1,q2,a,p1,r,f,g,Q,beta
REAL(8) :: invr

invr=1.D0/r

rhsq1p1=-(f*q1+g*p1)*invr+e*beta*q2+e*s2*beta*g*invr+0.25D0*e*s2*Q*a*a*invr*invr

END FUNCTION rhsq1p1
!*******************************************************************************************************************
REAL(8) FUNCTION q1p1v1(s2,q1,q2,a,r,f,g,Q,beta)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s2,q1,q2,a,r,f,g,Q,beta
REAL(8) :: invr

invr=1.D0/r

q1p1v1=-f*q1*invr+e*beta*q2+e*s2*beta*g*invr+0.25D0*e*s2*Q*a*a*invr*invr

END FUNCTION q1p1v1

!*******************************************************************************************************************
REAL(8) FUNCTION rhsq2p2(s1,q1,q2,a,p2,r,f,g,Q,beta)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s1,q1,q2,a,p2,r,f,g,Q,beta
REAL(8) :: invr

invr=1.D0/r

rhsq2p2=-(f*q2+g*p2)*invr-e*beta*q1-e*s1*beta*g*invr-0.25D0*e*s1*Q*a*a*invr*invr

END FUNCTION rhsq2p2
!*******************************************************************************************************************
REAL(8) FUNCTION q2p2v1(s1,q1,q2,a,r,f,g,Q,beta)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s1,q1,q2,a,r,f,g,Q,beta
REAL(8) :: invr

invr=1.D0/r

q2p2v1=-f*q2*invr-e*beta*q1-e*s1*beta*g*invr-0.25D0*e*s1*Q*a*a*invr*invr

END FUNCTION q2p2v1

!*******************************************************************************************************************
REAL(8) FUNCTION rhsyx(s1,s2,h,q1,q2,y,a,p1,p2,x,r,f,g,Q,beta)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s1,s2,h,q1,q2,y,a,p1,p2,x,r,f,g,Q,beta
REAL(8) :: invr,inva

invr=1.D0/r
inva=1.D0/a

rhsyx=-(f*y+g*x)*invr+0.25D0*alfa*DEXP(2.D0*alfa*h)*Q*Q*a*a*invr*invr*invr*invr&
 *(1.D0+32.D0*inva*inva*invr*gamma*DEXP(2.D0*alfa*h)*(6.D0*(0.25D0*a*a+f*g)-0.25D0*DEXP(2.D0*alfa*h)*Q*Q*a*a*invr*invr))&
 +(0.25D0*(alfa+1.D0)+16.D0*inva*inva*invr*(0.25D0*a*a+f*g)*gamma*alfa*DEXP(2.D0*alfa*h))*DEXP(2.D0*h*(alfa+1.D0))&
 *(p1*q1+p2*q2+e*beta*(s1*q2-s2*q1))+32.D0*inva*inva*invr*gamma*alfa*DEXP(2.D0*alfa*h)*((0.25D0*a*a+f*g)&
 *(2.D0*x*y-3.D0*(0.25D0*a*a+f*g)*invr*invr)&
 +r*r*(x*x*y*y+0.25D0*x*x*DEXP(2.D0*h*(alfa+1.D0))*(q1*q1+q2*q2)+0.25D0*DEXP(2.D0*h*(alfa+1.D0))&
 *(y*y+0.25D0*DEXP(2.D0*h*(alfa+1.D0))*(q1*q1+q2*q2))*(p1*p1+p2*p2+2.D0*e*beta*(s1*p2-s2*p1)+e*e*beta*beta*(s1*s1+s2*s2))))

END FUNCTION rhsyx

!*******************************************************************************************************************
REAL(8) FUNCTION rhsgf(h,a,r,f,g,Q)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: h,a,r,f,g,Q
REAL(8) :: invr

invr=1.D0/r

rhsgf=-(0.25D0*a*a+f*g)*invr+0.25D0*DEXP(2.D0*alfa*h)*Q*Q*a*a*invr*invr*invr

END FUNCTION rhsgf
!*******************************************************************************************************************
REAL(8) FUNCTION gfv1(h,a,r,Q)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: h,a,r,Q
REAL(8) :: invr

invr=1.D0/r

gfv1=-0.25D0*a*a*invr+0.25D0*DEXP(2.D0*alfa*h)*Q*Q*a*a*invr*invr*invr

END FUNCTION gfv1

!*******************************************************************************************************************
REAL(8) FUNCTION rhsg(h,q1,q2,y,d,r,g)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: h,q1,q2,y,d,r,g
REAL(8) :: invr

invr=1.D0/r

rhsg=2.D0*d*g-r*y*y-0.25D0*r*DEXP(2.D0*h*(alfa+1.D0))*(q1*q1+q2*q2)

END FUNCTION rhsg
!*******************************************************************************************************************
REAL(8) FUNCTION gv1(h,q1,q2,y)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: h,q1,q2,y

gv1=y*y+0.25D0*DEXP(2.D0*h*(alfa+1.D0))*(q1*q1+q2*q2)

END FUNCTION gv1

!*******************************************************************************************************************
REAL(8) FUNCTION rhsQ(s1,s2,h,q1,q2,y,r,Q)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s1,s2,h,q1,q2,y,r,Q

rhsQ=-2.D0*alfa*y*Q+0.5D0*e*r*r*DEXP(2.D0*h)*(s1*q2-s2*q1)

END FUNCTION rhsQ
!*******************************************************************************************************************
REAL(8) FUNCTION Qv1(s1,s2,h,q1,q2,r)
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s1,s2,h,q1,q2,r

Qv1=0.5D0*e*r*r*DEXP(2.D0*h)*(s1*q2-s2*q1)

END FUNCTION Qv1

!*******************************************************************************************************************
REAL(8) FUNCTION rhsbeta(a,r,Q)
IMPLICIT NONE
REAL(8),INTENT(IN) :: a,r,Q
REAL(8) :: invr

invr=1.D0/r

rhsbeta=0.5D0*Q*a*a*invr*invr

END FUNCTION rhsbeta

END MODULE modulerhs
