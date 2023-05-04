MODULE moduleWartosciFunkcjiAMRw1IF2

CONTAINS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!!! PROCEDURA zwracajaca wartosci funkcji na v(poczatekAMR) --> vAMR(1) jezeli licznikAMRN=2 !!!!!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE WartosciFunkcjiAMRw1IF2(podzAMR,licznikAMRW,&
 s1AMRNp,s2AMRNp,hAMRNp,aAMRNp,p1AMRNp,p2AMRNp,xAMRNp,fAMRNp,QAMRNp,betaAMRNp,&
 s1AMRNn,s2AMRNn,hAMRNn,aAMRNn,p1AMRNn,p2AMRNn,xAMRNn,fAMRNn,QAMRNn,betaAMRNn,&
 !OUT:
 s1AMRW,s2AMRW,hAMRW,aAMRW,p1AMRW,p2AMRW,xAMRW,fAMRW,QAMRW,betaAMRW)
IMPLICIT NONE
INTEGER,INTENT(IN) :: podzAMR,licznikAMRW
REAL(8),INTENT(IN) :: s1AMRNp,s2AMRNp,hAMRNp,aAMRNp,p1AMRNp,p2AMRNp,xAMRNp,fAMRNp,QAMRNp,betaAMRNp
REAL(8),INTENT(IN) :: s1AMRNn,s2AMRNn,hAMRNn,aAMRNn,p1AMRNn,p2AMRNn,xAMRNn,fAMRNn,QAMRNn,betaAMRNn
REAL(8),INTENT(OUT) :: s1AMRW,s2AMRW,hAMRW,aAMRW,p1AMRW,p2AMRW,xAMRW,fAMRW,QAMRW,betaAMRW

s1AMRW=s1AMRNp+(s1AMRNn-s1AMRNp)/podzAMR*(licznikAMRW-1)
s2AMRW=s2AMRNp+(s2AMRNn-s2AMRNp)/podzAMR*(licznikAMRW-1)
hAMRW=hAMRNp+(hAMRNn-hAMRNp)/podzAMR*(licznikAMRW-1)
aAMRW=aAMRNp+(aAMRNn-aAMRNp)/podzAMR*(licznikAMRW-1)
p1AMRW=p1AMRNp+(p1AMRNn-p1AMRNp)/podzAMR*(licznikAMRW-1)
p2AMRW=p2AMRNp+(p2AMRNn-p2AMRNp)/podzAMR*(licznikAMRW-1)
xAMRW=xAMRNp+(xAMRNn-xAMRNp)/podzAMR*(licznikAMRW-1)
fAMRW=fAMRNp+(fAMRNn-fAMRNp)/podzAMR*(licznikAMRW-1)
QAMRW=QAMRNp+(QAMRNn-QAMRNp)/podzAMR*(licznikAMRW-1)
betaAMRW=betaAMRNp+(betaAMRNn-betaAMRNp)/podzAMR*(licznikAMRW-1)

END SUBROUTINE WartosciFunkcjiAMRw1IF2

END MODULE moduleWartosciFunkcjiAMRw1IF2
