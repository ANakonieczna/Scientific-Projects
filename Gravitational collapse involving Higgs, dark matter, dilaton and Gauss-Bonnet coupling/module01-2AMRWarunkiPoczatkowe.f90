MODULE moduleWarunkiPoczatkoweAMR

CONTAINS
SUBROUTINE WarunkiPoczatkoweAMR(poczAMR,NAMR,&
 d,q1,q2,y,s1,s2,h,a,p1,p2,x,r,f,g,Q,beta,&
 !OUT:
 dAMR,q1AMR,q2AMR,yAMR,s1AMR,s2AMR,hAMR,aAMR,p1AMR,p2AMR,xAMR,rAMR,fAMR,gAMR,QAMR,betaAMR) 
IMPLICIT NONE
INTEGER, INTENT(IN) :: poczAMR,NAMR
REAL(8),DIMENSION(:),INTENT(IN) :: s1,s2,h,q1,q2,y,d,a,p1,p2,x,r,f,g,Q,beta
REAL(8),DIMENSION(:),INTENT(OUT) :: s1AMR,s2AMR,hAMR,q1AMR,q2AMR,yAMR,dAMR,aAMR,p1AMR,p2AMR,xAMR,rAMR,fAMR,gAMR,QAMR,betaAMR
!
INTEGER :: i

DO i=1,NAMR
   s1AMR(i)=s1(poczAMR+i-1)
   s2AMR(i)=s2(poczAMR+i-1)
   hAMR(i)=h(poczAMR+i-1)
   q1AMR(i)=q1(poczAMR+i-1)
   q2AMR(i)=q2(poczAMR+i-1)
   yAMR(i)=y(poczAMR+i-1)
   dAMR(i)=d(poczAMR+i-1)
   aAMR(i)=a(poczAMR+i-1)
   p1AMR(i)=p1(poczAMR+i-1)
   p2AMR(i)=p2(poczAMR+i-1)
   xAMR(i)=x(poczAMR+i-1)
   rAMR(i)=r(poczAMR+i-1)
   fAMR(i)=f(poczAMR+i-1)
   gAMR(i)=g(poczAMR+i-1)
   QAMR(i)=Q(poczAMR+i-1)
   betaAMR(i)=beta(poczAMR+i-1)
END DO

END SUBROUTINE WarunkiPoczatkoweAMR

END MODULE moduleWarunkiPoczatkoweAMR
