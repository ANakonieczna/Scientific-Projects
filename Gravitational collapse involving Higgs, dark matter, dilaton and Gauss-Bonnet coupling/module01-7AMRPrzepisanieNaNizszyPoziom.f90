MODULE modulePrzepisanieNaNizszyPoziomAMR

CONTAINS
SUBROUTINE PrzepisanieNaNizszyPoziomAMR(poczAMRW,NAMRW,&
 dW,yW,q1W,q2W,s1W,s2W,hW,aW,p1W,p2W,xW,rW,fW,gW,QW,betaW,&
 !OUT:
 dN,yN,q1N,q2N,s1N,s2N,hN,aN,p1N,p2N,xN,rN,fN,gN,QN,betaN)
IMPLICIT NONE
INTEGER, INTENT(IN) :: poczAMRW,NAMRW
REAL(8),DIMENSION(:),INTENT(IN) :: s1W,s2W,hW,q1W,q2W,yW,dW,aW,p1W,p2W,xW,rW,fW,gW,QW,betaW
REAL(8),DIMENSION(:),INTENT(OUT) :: s1N,s2N,hN,q1N,q2N,yN,dN,aN,p1N,p2N,xN,rN,fN,gN,QN,betaN
!
INTEGER :: i

DO i=1,NAMRW
   s1N(poczAMRW+i-1)=s1W(i)
   s2N(poczAMRW+i-1)=s2W(i)
   hN(poczAMRW+i-1)=hW(i)
   q1N(poczAMRW+i-1)=q1W(i)
   q2N(poczAMRW+i-1)=q2W(i)
   yN(poczAMRW+i-1)=yW(i)
   dN(poczAMRW+i-1)=dW(i)
   aN(poczAMRW+i-1)=aW(i)
   p1N(poczAMRW+i-1)=p1W(i)
   p2N(poczAMRW+i-1)=p2W(i)
   xN(poczAMRW+i-1)=xW(i)
   rN(poczAMRW+i-1)=rW(i)
   fN(poczAMRW+i-1)=fW(i)
   gN(poczAMRW+i-1)=gW(i)
   QN(poczAMRW+i-1)=QW(i)
   betaN(poczAMRW+i-1)=betaW(i)
END DO

END SUBROUTINE PrzepisanieNaNizszyPoziomAMR

END MODULE modulePrzepisanieNaNizszyPoziomAMR
