MODULE moduleObs

CONTAINS

!*******************************************************************************************************************
REAL(8) FUNCTION ObsED(s1,s2,h,q1,q2,y,a,p1,p2,x,r,Q,beta)
USE modulepochodne
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s1,s2,h,q1,q2,y,a,p1,p2,x,r,Q,beta
REAL(8) :: invr,inva

invr=1.D0/r
inva=1.D0/a

ObsED=&
(DEXP(2.D0*alfa*h)*Q*Q)*invr*invr*invr*invr+0.5D0*inva*inva*(DEXP(2.D0*(1.D0+alfa)*h)*(p1*p1+p2*p2+q1*q1+q2*q2)+&
2.D0*beta*DEXP(1.D0+2.D0*(1.D0+alfa)*h)*(p2*s1-p1*s2)+beta*beta*DEXP(2.D0*(1.D0+h+alfa*h))*(s1*s1+s2*s2)+4.D0*(x*x+y*y))

END FUNCTION ObsED

!*******************************************************************************************************************
REAL(8) FUNCTION ObsRP(s1,s2,h,q1,q2,y,a,p1,p2,x,r,Q,beta)
USE modulepochodne
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s1,s2,h,q1,q2,y,a,p1,p2,x,r,Q,beta
REAL(8) :: invr,inva

invr=1.D0/r
inva=1.D0/a

ObsRP=&
-((DEXP(2.D0*alfa*h)*Q*Q)*invr*invr*invr*invr)+0.5D0*inva*inva*(DEXP(2.D0*(1.D0+alfa)*h)*(p1*p1+p2*p2+q1*q1+q2*q2)+&
2.D0*beta*DEXP(1.D0+2.D0*(1.D0+alfa)*h)*(p2*s1-p1*s2)+beta*beta*DEXP(2.D0*(1.D0+h+alfa*h))*(s1*s1+s2*s2)+4.D0*(x*x+y*y))

END FUNCTION ObsRP

!*******************************************************************************************************************
REAL(8) FUNCTION ObsPA(s1,s2,h,q1,q2,y,a,p1,p2,x,r,Q,beta)
USE modulepochodne
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e
COMMON /PARAMSmodel/ alfa,gamma,e
!*************************
REAL(8),INTENT(IN) :: s1,s2,h,q1,q2,y,a,p1,p2,x,r,Q,beta
REAL(8) :: invr,inva

invr=1.D0/r
inva=1.D0/a

ObsPA=&
(2.D0*DEXP(2.D0*alfa*h)*Q*Q)*invr*invr*invr*invr-(DEXP(2.D0*(1.D0+alfa)*h)*(p1*p1+p2*p2-2.D0*p1*q1+q1*q1-2.D0*p2*q2+q2*q2)+&
2.D0*beta*DEXP(1.D0+2.D0*(1.D0+alfa)*h)*(p2*s1-q2*s1-p1*s2+q1*s2)+beta*beta*DEXP(2.D0*(1.D0+h+alfa*h))*(s1*s1+s2*s2)+&
4.D0*(x-y)*(x-y))*0.5D0*inva*inva

END FUNCTION ObsPA

!*******************************************************************************************************************
REAL(8) FUNCTION ObsLT(h0,ap2,a,an)
USE modulepochodne
IMPLICIT NONE
REAL(8),INTENT(IN) :: h0,ap2,a,an
REAL(8) :: inva,aPOu

inva=1.D0/a
!
aPOu=DerivSym3pts(h0,ap2,an)

ObsLT=aPOu*inva*inva

END FUNCTION ObsLT

END MODULE moduleObs
