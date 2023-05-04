MODULE moduleRK2k1234u

CONTAINS
SUBROUTINE k1234u(s1,s2,h,q1,q2,y,a,p1,p2,x,r,f,g,Q,beta,&
 !OUT:
 kd,kq1,kq2,ky)
!
USE modulerhs
IMPLICIT NONE
REAL(8),INTENT(IN) :: s1,s2,h,q1,q2,y,a,p1,p2,x,r,f,g,Q,beta
REAL(8),INTENT(OUT) :: kd,kq1,kq2,ky

!* RK2-k
kd=rhsd(s1,s2,h,q1,q2,y,a,p1,p2,x,r,f,g,Q,beta)
kq1=rhsq1p1(s2,q1,q2,a,p1,r,f,g,Q,beta)
kq2=rhsq2p2(s1,q1,q2,a,p2,r,f,g,Q,beta)
ky=rhsyx(s1,s2,h,q1,q2,y,a,p1,p2,x,r,f,g,Q,beta)

END SUBROUTINE k1234u

END MODULE moduleRK2k1234u
