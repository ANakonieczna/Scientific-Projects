MODULE moduleWartosciFunkcjiAMRw1IF1

CONTAINS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!!! PROCEDURA zwracajaca wartosci funkcji na v(poczatekAMR) --> vAMR(1) jezeli licznikAMRW=podzialAMR+1 !!!!!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE WartosciFunkcjiAMRw1IF1(&
 dAMRN,q1AMRN,q2AMRN,yAMRN,s1AMRN,s2AMRN,hAMRN,aAMRN,p1AMRN,p2AMRN,xAMRN,rAMRN,fAMRN,gAMRN,QAMRN,betaAMRN,&
 !OUT:
 dAMRW,q1AMRW,q2AMRW,yAMRW,s1AMRW,s2AMRW,hAMRW,aAMRW,p1AMRW,p2AMRW,xAMRW,rAMRW,fAMRW,gAMRW,QAMRW,betaAMRW)
IMPLICIT NONE
REAL(8),INTENT(IN) :: s1AMRN,s2AMRN,hAMRN,q1AMRN,q2AMRN,yAMRN,dAMRN,aAMRN,p1AMRN,p2AMRN,xAMRN,rAMRN,fAMRN,gAMRN,QAMRN,betaAMRN
REAL(8),INTENT(OUT) :: s1AMRW,s2AMRW,hAMRW,q1AMRW,q2AMRW,yAMRW,dAMRW,aAMRW,p1AMRW,p2AMRW,xAMRW,rAMRW,fAMRW,gAMRW,QAMRW,betaAMRW

s1AMRW=s1AMRN
s2AMRW=s2AMRN
hAMRW=hAMRN
q1AMRW=q1AMRN
q2AMRW=q2AMRN
yAMRW=yAMRN
dAMRW=dAMRN
aAMRW=aAMRN
p1AMRW=p1AMRN
p2AMRW=p2AMRN
xAMRW=xAMRN
rAMRW=rAMRN
fAMRW=fAMRN
gAMRW=gAMRN
QAMRW=QAMRN
betaAMRW=betaAMRN

END SUBROUTINE WartosciFunkcjiAMRw1IF1

END MODULE moduleWartosciFunkcjiAMRw1IF1
