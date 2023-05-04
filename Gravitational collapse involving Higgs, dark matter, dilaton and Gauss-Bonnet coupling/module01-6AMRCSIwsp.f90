MODULE moduleCSIwsp

CONTAINS
SUBROUTINE CubicSplineInterpolationWSP(h,N,funCSI,&
 !OUT:
 aCSI,bCSI,cCSI,dCSI)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h
INTEGER,INTENT(IN) :: N
REAL(8),DIMENSION(:),INTENT(IN) :: funCSI
REAL(8),DIMENSION(:),INTENT(OUT) :: aCSI,bCSI,cCSI,dCSI
!
INTEGER :: i
REAL(8) :: a(N),l(N),m(N),z(N)

l(1)=1.D0
m(1)=0.D0
z(1)=0.D0
DO i=2,N-1
   a(i)=3.D0/h*(funCSI(i-1)-2.D0*funCSI(i)+funCSI(i+1))
   l(i)=h*(4.D0-m(i-1))
   m(i)=h/l(i)
   z(i)=(a(i)-h*z(i-1))/l(i)
END DO
l(N)=1.D0
z(N)=0.D0
cCSI(N)=0.D0
DO i=N-1,1,-1
   aCSI(i)=funCSI(i)
   cCSI(i)=z(i)-m(i)*cCSI(i+1)
   bCSI(i)=(aCSI(i+1)-aCSI(i))/h-h/3.D0*(cCSI(i+1)+2.D0*cCSI(i))
   dCSI(i)=(cCSI(i+1)-cCSI(i))/(3.D0*h)
END DO
aCSI(N)=funCSI(N)

END SUBROUTINE CubicSplineInterpolationWSP

END MODULE moduleCSIwsp
