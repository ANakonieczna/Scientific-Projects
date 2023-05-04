MODULE moduleprzepisanie

CONTAINS
SUBROUTINE Przepisanie(&
 dn,yn,q1n,q2n,s1n,s2n,hn,an,p1n,p2n,xn,rn,fn,gn,Qn,betan,&
 !OUT:
 dp,yp,q1p,q2p,s1p,s2p,hp,ap,p1p,p2p,xp,rp,fp,gp,Qp,betap,&
 dp2,q1p2,q2p2,yp2,s1p2,s2p2,hp2,ap2,p1p2,p2p2,xp2,rp2,fp2,gp2,Qp2,betap2)
IMPLICIT NONE
REAL(8),DIMENSION(:),INTENT(IN) :: s1n,s2n,hn,q1n,q2n,yn,dn,an,p1n,p2n,xn,rn,fn,gn,Qn,betan
REAL(8),DIMENSION(:),INTENT(INOUT) :: s1p,s2p,hp,q1p,q2p,yp,dp,ap,p1p,p2p,xp,rp,fp,gp,Qp,betap
REAL(8),DIMENSION(:),INTENT(OUT) :: s1p2,s2p2,hp2,q1p2,q2p2,yp2,dp2,ap2,p1p2,p2p2,xp2,rp2,fp2,gp2,Qp2,betap2

s1p2=s1p
s2p2=s2p
hp2=hp
q1p2=q1p
q2p2=q2p
yp2=yp
dp2=dp
ap2=ap
p1p2=p1p
p2p2=p2p
xp2=xp
rp2=rp
fp2=fp
gp2=gp
Qp2=Qp
betap2=betap

s1p=s1n
s2p=s2n
hp=hn
q1p=q1n
q2p=q2n
yp=yn
dp=dn
ap=an
p1p=p1n
p2p=p2n
xp=xn
rp=rn
fp=fn
gp=gn
Qp=Qn
betap=betan

END SUBROUTINE Przepisanie

END MODULE moduleprzepisanie
