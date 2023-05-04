Program DilLoveCollapse   ! s1,s2,h,q1,q2,y,d,a,p1,p2,x,r,f,g,Q,beta

USE moduleWarunkiBrzegowe
USE moduleWarunkiPoczatkowe
USE moduleABM2ewolucjaV
USE moduleRK2ewolucjaU
USE moduleprzepisanie
!
USE moduleZnajdzKoniecAMR
USE moduleWarunkiPoczatkoweAMR
USE moduleTworzenieTablicyFUNpppn
USE moduleWartosciFunkcjiAMRw1IF1
USE moduleWartosciFunkcjiAMRw1IF2
USE moduleCSIwsp
USE modulePrzepisanieNaNizszyPoziomAMR
!
USE modulepochodne
USE moduleObs

IMPLICIT NONE

!******* PARAMETRY *******
REAL(8) :: alfa,gamma,e                                          !parametry modelu
REAL(8) :: ps,ph,v0h,Dh,vi,vf                                    !parametry profili poczatkowych
REAL(8) :: pi,phdiff                                             !pi i pi/2
REAL(8) :: r0ini                                                 !wartosc r w u=v=0
!
REAL(8) :: bladAMR
INTEGER :: podzialAMR
!
COMMON /PARAMSmodel/ alfa,gamma,e
COMMON /PARAMSprofile/ ps,ph,v0h,Dh,vi,vf,pi,phdiff
COMMON /PARAMSini/ r0ini
COMMON /PARAMSamr/ bladAMR,podzialAMR
!
REAL(8) :: h0                                                    !krok w v
INTEGER :: N,Nkoniec                                             !liczba pktow siatki w kierunku v, koncowa wartosc u
!*************************

REAL(8) :: u,uprev
INTEGER :: i,j
INTEGER :: licznik
REAL(8) :: R0,Rconst,R,H,gPOv,gPOu,fPOu,fPOv,mEH
INTEGER :: linieRconst,coile3d

REAL(8),ALLOCATABLE,DIMENSION(:) :: v
REAL(8),ALLOCATABLE,DIMENSION(:) :: dprev2,q1prev2,q2prev2,yprev2,s1prev2,s2prev2,hprev2,aprev2,p1prev2,p2prev2,xprev2,rprev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: fprev2,gprev2,Qprev2,betaprev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: dprev,q1prev,q2prev,yprev,s1prev,s2prev,hprev,aprev,p1prev,p2prev,xprev,rprev
REAL(8),ALLOCATABLE,DIMENSION(:) :: fprev,gprev,Qprev,betaprev
REAL(8),ALLOCATABLE,DIMENSION(:) :: dnext,q1next,q2next,ynext,s1next,s2next,hnext,anext,p1next,p2next,xnext,rnext
REAL(8),ALLOCATABLE,DIMENSION(:) :: fnext,gnext,Qnext,betanext
REAL(8),ALLOCATABLE,DIMENSION(:) :: R4v,R4u
INTEGER,ALLOCATABLE,DIMENSION(:) :: czyR4

!!!!!!! AMR !!!!!!!
REAL(8) :: funkcjableduAMR
INTEGER :: iAMR0,iAMR1,iAMR2,iAMR3,iAMR4
REAL(8) :: uAMR1,uAMR2,uAMR3,uAMR4,uAMR5,hAMR1,hAMR2,hAMR3,hAMR4,hAMR5
INTEGER :: NAMR1,NAMR2,NAMR3,NAMR4,NAMR5
INTEGER :: poziomAMR,licznikAMR1,licznikAMR2,licznikAMR3,licznikAMR4,licznikAMR5
INTEGER :: poczatekAMR1,koniecAMR1,poczatekAMR2,koniecAMR2,poczatekAMR3,koniecAMR3,poczatekAMR4,koniecAMR4,poczatekAMR5,koniecAMR5
INTEGER :: koniecAMR_minus_poczatekAMR1,koniecAMR_minus_poczatekAMR2,koniecAMR_minus_poczatekAMR3,koniecAMR_minus_poczatekAMR4
INTEGER :: koniecAMR_minus_poczatekAMR5
!
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR1prev2,q1AMR1prev2,q2AMR1prev2,yAMR1prev2,s1AMR1prev2,s2AMR1prev2,hAMR1prev2,aAMR1prev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR1prev2,p2AMR1prev2,xAMR1prev2,rAMR1prev2,fAMR1prev2,gAMR1prev2,QAMR1prev2,betaAMR1prev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR1prev,q1AMR1prev,q2AMR1prev,yAMR1prev,s1AMR1prev,s2AMR1prev,hAMR1prev,aAMR1prev
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR1prev,p2AMR1prev,xAMR1prev,rAMR1prev,fAMR1prev,gAMR1prev,QAMR1prev,betaAMR1prev
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR1next,q1AMR1next,q2AMR1next,yAMR1next,s1AMR1next,s2AMR1next,hAMR1next,aAMR1next
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR1next,p2AMR1next,xAMR1next,rAMR1next,fAMR1next,gAMR1next,QAMR1next,betaAMR1next
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR2prev2,q1AMR2prev2,q2AMR2prev2,yAMR2prev2,s1AMR2prev2,s2AMR2prev2,hAMR2prev2,aAMR2prev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR2prev2,p2AMR2prev2,xAMR2prev2,rAMR2prev2,fAMR2prev2,gAMR2prev2,QAMR2prev2,betaAMR2prev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR2prev,q1AMR2prev,q2AMR2prev,yAMR2prev,s1AMR2prev,s2AMR2prev,hAMR2prev,aAMR2prev
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR2prev,p2AMR2prev,xAMR2prev,rAMR2prev,fAMR2prev,gAMR2prev,QAMR2prev,betaAMR2prev
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR2next,q1AMR2next,q2AMR2next,yAMR2next,s1AMR2next,s2AMR2next,hAMR2next,aAMR2next
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR2next,p2AMR2next,xAMR2next,rAMR2next,fAMR2next,gAMR2next,QAMR2next,betaAMR2next
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR3prev2,q1AMR3prev2,q2AMR3prev2,yAMR3prev2,s1AMR3prev2,s2AMR3prev2,hAMR3prev2,aAMR3prev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR3prev2,p2AMR3prev2,xAMR3prev2,rAMR3prev2,fAMR3prev2,gAMR3prev2,QAMR3prev2,betaAMR3prev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR3prev,q1AMR3prev,q2AMR3prev,yAMR3prev,s1AMR3prev,s2AMR3prev,hAMR3prev,aAMR3prev
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR3prev,p2AMR3prev,xAMR3prev,rAMR3prev,fAMR3prev,gAMR3prev,QAMR3prev,betaAMR3prev
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR3next,q1AMR3next,q2AMR3next,yAMR3next,s1AMR3next,s2AMR3next,hAMR3next,aAMR3next
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR3next,p2AMR3next,xAMR3next,rAMR3next,fAMR3next,gAMR3next,QAMR3next,betaAMR3next
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR4prev2,q1AMR4prev2,q2AMR4prev2,yAMR4prev2,s1AMR4prev2,s2AMR4prev2,hAMR4prev2,aAMR4prev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR4prev2,p2AMR4prev2,xAMR4prev2,rAMR4prev2,fAMR4prev2,gAMR4prev2,QAMR4prev2,betaAMR4prev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR4prev,q1AMR4prev,q2AMR4prev,yAMR4prev,s1AMR4prev,s2AMR4prev,hAMR4prev,aAMR4prev
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR4prev,p2AMR4prev,xAMR4prev,rAMR4prev,fAMR4prev,gAMR4prev,QAMR4prev,betaAMR4prev
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR4next,q1AMR4next,q2AMR4next,yAMR4next,s1AMR4next,s2AMR4next,hAMR4next,aAMR4next
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR4next,p2AMR4next,xAMR4next,rAMR4next,fAMR4next,gAMR4next,QAMR4next,betaAMR4next
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR5prev2,q1AMR5prev2,q2AMR5prev2,yAMR5prev2,s1AMR5prev2,s2AMR5prev2,hAMR5prev2,aAMR5prev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR5prev2,p2AMR5prev2,xAMR5prev2,rAMR5prev2,fAMR5prev2,gAMR5prev2,QAMR5prev2,betaAMR5prev2
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR5prev,q1AMR5prev,q2AMR5prev,yAMR5prev,s1AMR5prev,s2AMR5prev,hAMR5prev,aAMR5prev
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR5prev,p2AMR5prev,xAMR5prev,rAMR5prev,fAMR5prev,gAMR5prev,QAMR5prev,betaAMR5prev
REAL(8),ALLOCATABLE,DIMENSION(:) :: dAMR5next,q1AMR5next,q2AMR5next,yAMR5next,s1AMR5next,s2AMR5next,hAMR5next,aAMR5next
REAL(8),ALLOCATABLE,DIMENSION(:) :: p1AMR5next,p2AMR5next,xAMR5next,rAMR5next,fAMR5next,gAMR5next,QAMR5next,betaAMR5next
REAL(8) :: s1AMRCSI(3),s2AMRCSI(3),hAMRCSI(3),aAMRCSI(3),p1AMRCSI(3),p2AMRCSI(3),xAMRCSI(3),fAMRCSI(3),QAMRCSI(3),betaAMRCSI(3)
REAL(8) :: aCSI(3),bCSI(3),cCSI(3),dCSI(3)
REAL(8) :: coef2,coef3
!!!!!!!!!!!!!!!!!!!


OPEN(1,file='aParameters.data')
!OPEN(2,file='R0.dat')                 !u,v,r0
!OPEN(3,file='staler-v.dat')           !u,v,r linie
OPEN(4,file='ah-v.dat')               !u,v,r,gPOv,gPOu dla g=0 (horyzont pozorny-1)
!OPEN(5,file='ah-u.dat')               !u,v,r,fPOu,fPOv dla f=0 (horyzont pozorny-2)
!OPEN(6,file='staler-u.dat')           !u,v,r linie
!OPEN(7,file='obs.dat')                !u,v,ed,rp,pa
!OPEN(8,file='R4u.dat')
!OPEN(9,file='staler.dat')             !u,v,r w rownych odstepach
!OPEN(88,file='amr.dat')


!******* PARAMETRY ********************************************************************************
READ(1,*) alfa
READ(1,*) gamma
READ(1,*) e
READ(1,*) ps
READ(1,*) ph
READ(1,*) v0h
READ(1,*) Dh
READ(1,*) vi
READ(1,*) vf
READ(1,*) r0ini
READ(1,*) h0
READ(1,*) N
READ(1,*) Nkoniec
READ(1,*) pi
READ(1,*) R0
READ(1,*) Rconst
READ(1,*) linieRconst
READ(1,*) bladAMR
READ(1,*) podzialAMR
READ(1,*) coile3d

phdiff=0.5D0*pi
!**************************************************************************************************

ALLOCATE(v(N),&
 dprev2(N),q1prev2(N),q2prev2(N),yprev2(N),s1prev2(N),s2prev2(N),hprev2(N),aprev2(N),p1prev2(N),p2prev2(N),xprev2(N),rprev2(N),&
 fprev2(N),gprev2(N),Qprev2(N),betaprev2(N),&
 dprev(N),q1prev(N),q2prev(N),yprev(N),s1prev(N),s2prev(N),hprev(N),aprev(N),p1prev(N),p2prev(N),xprev(N),rprev(N),&
 fprev(N),gprev(N),Qprev(N),betaprev(N),&
 dnext(N),q1next(N),q2next(N),ynext(N),s1next(N),s2next(N),hnext(N),anext(N),p1next(N),p2next(N),xnext(N),rnext(N),&
 fnext(N),gnext(N),Qnext(N),betanext(N),&
 R4v(linieRconst),R4u(3*linieRconst+1),czyR4(linieRconst))

 
!******* WARTOSCI v *******
DO i=1,N
   v(i)=(i-1)*h0
END DO

u=0.D0
uprev=u
licznik=1

CALL WarunkiPoczatkowe(h0,N,Nkoniec,v,&
 !OUT:
 dprev,q1prev,q2prev,yprev,s1prev,s2prev,hprev,aprev,p1prev,p2prev,xprev,rprev,&
 fprev,gprev,Qprev,betaprev)


!******* wczytanie wartosci R4u z pliku ***********************************************************
!DO i=1,3*linieRconst+1
!   READ(8,*) R4u(i)
!END DO

!******* wypisanie do plikow wartosci funkcji z osi u=0 *******************************************
!DO i=1,linieRconst
!   czyR4(i)=INT(i*Nkoniec/linieRconst)
!END DO
!
!DO i=1,Nkoniec
! DO j=1,linieRconst
!    IF (i.EQ.czyR4(j)) THEN
!       R4v(j)=rprev(i)
!       WRITE(3,30) u,v(i),rprev(i)
!    END IF
! END DO
!END DO
!
!WRITE(6,30) u,v(1),rprev(1)


!******* WARUNKI BRZEGOWE *******
s1next(1)=0.D0
s2next(1)=0.D0
hnext(1)=0.D0
anext(1)=1.D0
Qnext(1)=0.D0
betanext(1)=0.D0
fnext(1)=-0.5D0
p1next(1)=0.D0
p2next(1)=0.D0
xnext(1)=0.D0

!**************************************************************************************************
!******* PETLA GLOWNA *****************************************************************************
!**************************************************************************************************
DO licznik=2,INT(3.D0*Nkoniec)

u=u+h0

CALL EwolucjaU(h0,h0,N,1,.FALSE.,0,&
 dprev,q1prev,q2prev,yprev,s1prev,s2prev,hprev,aprev,p1prev,p2prev,xprev,rprev,fprev,gprev,Qprev,betaprev,&
 0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,&
 0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,&
 !OUT:
 dnext,q1next,q2next,ynext)

CALL WarunkiBrzegowe(h0,&
 rprev(1),gprev(1),hnext(1),anext(1),fnext(1),Qnext(1),&
 !OUT:
 rnext(1),gnext(1))
 
CALL EwolucjaV(h0,N,1,&
 dnext,q1next,q2next,ynext,&
 !OUT:
 s1next,s2next,hnext,anext,p1next,p2next,xnext,rnext,fnext,gnext,Qnext,betanext)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! SIATKA SAMOADAPTACYJNA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  *!*!*!*!* KONTROLA BLEDU WZDLUZ u=const. DO SIATKI SAMOADAPTACYJNEJ *!*!*!*!*
DO iAMR0=2,N
   funkcjableduAMR=ABS((rnext(iAMR0)-rprev(iAMR0))/rprev(iAMR0))
   IF (funkcjableduAMR.GE.bladAMR) THEN
      poziomAMR=1
      poczatekAMR1=iAMR0-1
      CALL ZnajdzKoniecAMR(N,bladAMR,rprev,rnext,poczatekAMR1,&
       !OUT:
       koniecAMR1)

!WRITE(88,*) 'entering AMR'
!WRITE(88,*) licznik
!WRITE(88,*) u

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! POZIOM AMR1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
koniecAMR_minus_poczatekAMR1=koniecAMR1-poczatekAMR1
hAMR1=h0/podzialAMR
NAMR1=koniecAMR_minus_poczatekAMR1+1

ALLOCATE(dAMR1prev2(NAMR1),q1AMR1prev2(NAMR1),q2AMR1prev2(NAMR1),yAMR1prev2(NAMR1),s1AMR1prev2(NAMR1),s2AMR1prev2(NAMR1),&
 hAMR1prev2(NAMR1),aAMR1prev2(NAMR1),p1AMR1prev2(NAMR1),p2AMR1prev2(NAMR1),xAMR1prev2(NAMR1),rAMR1prev2(NAMR1),&
 fAMR1prev2(NAMR1),gAMR1prev2(NAMR1),QAMR1prev2(NAMR1),betaAMR1prev2(NAMR1),&
 dAMR1prev(NAMR1),q1AMR1prev(NAMR1),q2AMR1prev(NAMR1),yAMR1prev(NAMR1),s1AMR1prev(NAMR1),s2AMR1prev(NAMR1),&
 hAMR1prev(NAMR1),aAMR1prev(NAMR1),p1AMR1prev(NAMR1),p2AMR1prev(NAMR1),xAMR1prev(NAMR1),rAMR1prev(NAMR1),&
 fAMR1prev(NAMR1),gAMR1prev(NAMR1),QAMR1prev(NAMR1),betaAMR1prev(NAMR1),&
 dAMR1next(NAMR1),q1AMR1next(NAMR1),q2AMR1next(NAMR1),yAMR1next(NAMR1),s1AMR1next(NAMR1),s2AMR1next(NAMR1),&
 hAMR1next(NAMR1),aAMR1next(NAMR1),p1AMR1next(NAMR1),p2AMR1next(NAMR1),xAMR1next(NAMR1),rAMR1next(NAMR1),&
 fAMR1next(NAMR1),gAMR1next(NAMR1),QAMR1next(NAMR1),betaAMR1next(NAMR1))

uAMR1=0.D0
licznikAMR1=1

!!!!!!! WARUNKI POCZATKOWE MODULU AMR NA OSI uAMR=0 - PRZEPISANIE WARTOSCI !!!!!!!
CALL WarunkiPoczatkoweAMR(poczatekAMR1,NAMR1,&
 dprev,q1prev,q2prev,yprev,s1prev,s2prev,hprev,aprev,p1prev,p2prev,xprev,rprev,fprev,gprev,Qprev,betaprev,&
 !OUT:
 dAMR1prev,q1AMR1prev,q2AMR1prev,yAMR1prev,s1AMR1prev,s2AMR1prev,hAMR1prev,aAMR1prev,p1AMR1prev,p2AMR1prev,xAMR1prev,rAMR1prev,&
 fAMR1prev,gAMR1prev,QAMR1prev,betaAMR1prev)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! PETLA GLOWNA AMR1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO licznikAMR1=2,podzialAMR+1
   uAMR1=uAMR1+hAMR1
!  !!!!! WARUNKI BRZEGOWE MODULU AMR NA v(poczatekAMR) !!!!!
   IF (licznikAMR1.EQ.podzialAMR+1) THEN
      CALL WartosciFunkcjiAMRw1IF1(&
       dnext(poczatekAMR1),q1next(poczatekAMR1),q2next(poczatekAMR1),ynext(poczatekAMR1),&
       s1next(poczatekAMR1),s2next(poczatekAMR1),hnext(poczatekAMR1),anext(poczatekAMR1),&
       p1next(poczatekAMR1),p2next(poczatekAMR1),xnext(poczatekAMR1),rnext(poczatekAMR1),fnext(poczatekAMR1),gnext(poczatekAMR1),&
       Qnext(poczatekAMR1),betanext(poczatekAMR1),&
       !OUT:
       dAMR1next(1),q1AMR1next(1),q2AMR1next(1),yAMR1next(1),s1AMR1next(1),s2AMR1next(1),hAMR1next(1),aAMR1next(1),&
       p1AMR1next(1),p2AMR1next(1),xAMR1next(1),rAMR1next(1),fAMR1next(1),gAMR1next(1),&
       QAMR1next(1),betaAMR1next(1))
    ELSE
      IF (licznik.EQ.2) THEN
         CALL WartosciFunkcjiAMRw1IF2(podzialAMR,licznikAMR1,&
          s1prev(poczatekAMR1),s2prev(poczatekAMR1),hprev(poczatekAMR1),aprev(poczatekAMR1),&
          p1prev(poczatekAMR1),p2prev(poczatekAMR1),xprev(poczatekAMR1),fprev(poczatekAMR1),&
          Qprev(poczatekAMR1),betaprev(poczatekAMR1),&
          s1next(poczatekAMR1),s2next(poczatekAMR1),hnext(poczatekAMR1),anext(poczatekAMR1),&
          p1next(poczatekAMR1),p2next(poczatekAMR1),xnext(poczatekAMR1),fnext(poczatekAMR1),&
          Qnext(poczatekAMR1),betanext(poczatekAMR1),&
          !OUT:
          s1AMR1next(1),s2AMR1next(1),hAMR1next(1),aAMR1next(1),p1AMR1next(1),p2AMR1next(1),xAMR1next(1),fAMR1next(1),&
          QAMR1next(1),betaAMR1next(1))
    ELSE
      !  TWORZENIE TABLIC INTERPOLACYJNYCH CSI !
      CALL TworzenieTablicyFUNpppn(s1prev2(poczatekAMR1),s1prev(poczatekAMR1),s1next(poczatekAMR1),s1AMRCSI)
      CALL TworzenieTablicyFUNpppn(s2prev2(poczatekAMR1),s2prev(poczatekAMR1),s2next(poczatekAMR1),s2AMRCSI)
      CALL TworzenieTablicyFUNpppn(hprev2(poczatekAMR1),hprev(poczatekAMR1),hnext(poczatekAMR1),hAMRCSI)
      CALL TworzenieTablicyFUNpppn(aprev2(poczatekAMR1),aprev(poczatekAMR1),anext(poczatekAMR1),aAMRCSI)
      CALL TworzenieTablicyFUNpppn(p1prev2(poczatekAMR1),p1prev(poczatekAMR1),p1next(poczatekAMR1),p1AMRCSI)
      CALL TworzenieTablicyFUNpppn(p2prev2(poczatekAMR1),p2prev(poczatekAMR1),p2next(poczatekAMR1),p2AMRCSI)
      CALL TworzenieTablicyFUNpppn(xprev2(poczatekAMR1),xprev(poczatekAMR1),xnext(poczatekAMR1),xAMRCSI)
      CALL TworzenieTablicyFUNpppn(fprev2(poczatekAMR1),fprev(poczatekAMR1),fnext(poczatekAMR1),fAMRCSI)
      CALL TworzenieTablicyFUNpppn(Qprev2(poczatekAMR1),Qprev(poczatekAMR1),Qnext(poczatekAMR1),QAMRCSI)
      CALL TworzenieTablicyFUNpppn(betaprev2(poczatekAMR1),betaprev(poczatekAMR1),betanext(poczatekAMR1),betaAMRCSI)
      !  INTERPOLACJA CSI !
      coef2=uAMR1*uAMR1
      coef3=coef2*uAMR1
      CALL CubicSplineInterpolationWSP(h0,3,s1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s1AMR1next(1)=aCSI(2)+bCSI(2)*uAMR1+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0,3,s2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s2AMR1next(1)=aCSI(2)+bCSI(2)*uAMR1+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0,3,hAMRCSI,aCSI,bCSI,cCSI,dCSI)
      hAMR1next(1)=aCSI(2)+bCSI(2)*uAMR1+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0,3,aAMRCSI,aCSI,bCSI,cCSI,dCSI)
      aAMR1next(1)=aCSI(2)+bCSI(2)*uAMR1+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0,3,p1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p1AMR1next(1)=aCSI(2)+bCSI(2)*uAMR1+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0,3,p2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p2AMR1next(1)=aCSI(2)+bCSI(2)*uAMR1+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0,3,xAMRCSI,aCSI,bCSI,cCSI,dCSI)
      xAMR1next(1)=aCSI(2)+bCSI(2)*uAMR1+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0,3,fAMRCSI,aCSI,bCSI,cCSI,dCSI)
      fAMR1next(1)=aCSI(2)+bCSI(2)*uAMR1+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0,3,QAMRCSI,aCSI,bCSI,cCSI,dCSI)
      QAMR1next(1)=aCSI(2)+bCSI(2)*uAMR1+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0,3,betaAMRCSI,aCSI,bCSI,cCSI,dCSI)
      betaAMR1next(1)=aCSI(2)+bCSI(2)*uAMR1+cCSI(2)*coef2+dCSI(2)*coef3
      END IF
   END IF

!  !!!!! OBLICZENIA WZDLUZ uAMR=const. !!!!!!!
   CALL EwolucjaU(h0,hAMR1,NAMR1,1,.TRUE.,licznik,&
    dAMR1prev,q1AMR1prev,q2AMR1prev,yAMR1prev,s1AMR1prev,s2AMR1prev,hAMR1prev,aAMR1prev,p1AMR1prev,p2AMR1prev,xAMR1prev,rAMR1prev,&
    fAMR1prev,gAMR1prev,QAMR1prev,betaAMR1prev,&
    s1AMR1prev2(1),s2AMR1prev2(1),hAMR1prev2(1),aAMR1prev2(1),p1AMR1prev2(1),p2AMR1prev2(1),xAMR1prev2(1),fAMR1prev2(1),&
    QAMR1prev2(1),betaAMR1prev2(1),&
    s1AMR1next(1),s2AMR1next(1),hAMR1next(1),aAMR1next(1),p1AMR1next(1),p2AMR1next(1),xAMR1next(1),fAMR1next(1),&
    QAMR1next(1),betaAMR1next(1),&
    !OUT:
    dAMR1next,q1AMR1next,q2AMR1next,yAMR1next)
   !
   IF (licznikAMR1.NE.podzialAMR+1) THEN
      CALL WarunkiBrzegowe(hAMR1,&
       rAMR1prev(1),gAMR1prev(1),hAMR1next(1),aAMR1next(1),fAMR1next(1),QAMR1next(1),&
       !OUT:
       rAMR1next(1),gAMR1next(1))
   END IF
   !
   CALL EwolucjaV(h0,NAMR1,1,&
    dAMR1next,q1AMR1next,q2AMR1next,yAMR1next,&
    !OUT:
    s1AMR1next,s2AMR1next,hAMR1next,aAMR1next,p1AMR1next,p2AMR1next,xAMR1next,rAMR1next,fAMR1next,gAMR1next,QAMR1next,betaAMR1next)

!  *!*!*!* KONTROLA BLEDU WZDLUZ uAMR=const. *!*!*!*
DO iAMR1=2,NAMR1
   funkcjableduAMR=ABS((rAMR1next(iAMR1)-rAMR1prev(iAMR1))/rAMR1prev(iAMR1))
   IF (funkcjableduAMR.GE.bladAMR) THEN
      poziomAMR=2
      poczatekAMR2=iAMR1-1
      CALL ZnajdzKoniecAMR(NAMR1,bladAMR,rAMR1prev,rAMR1next,poczatekAMR2,&
      !OUT:
      koniecAMR2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! POZIOM AMR2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
koniecAMR_minus_poczatekAMR2=koniecAMR2-poczatekAMR2
hAMR2=hAMR1/podzialAMR
NAMR2=koniecAMR_minus_poczatekAMR2+1

ALLOCATE(dAMR2prev2(NAMR2),q1AMR2prev2(NAMR2),q2AMR2prev2(NAMR2),yAMR2prev2(NAMR2),s1AMR2prev2(NAMR2),s2AMR2prev2(NAMR2),&
 hAMR2prev2(NAMR2),aAMR2prev2(NAMR2),p1AMR2prev2(NAMR2),p2AMR2prev2(NAMR2),xAMR2prev2(NAMR2),rAMR2prev2(NAMR2),&
 fAMR2prev2(NAMR2),gAMR2prev2(NAMR2),QAMR2prev2(NAMR2),betaAMR2prev2(NAMR2),&
 dAMR2prev(NAMR2),q1AMR2prev(NAMR2),q2AMR2prev(NAMR2),yAMR2prev(NAMR2),s1AMR2prev(NAMR2),s2AMR2prev(NAMR2),&
 hAMR2prev(NAMR2),aAMR2prev(NAMR2),p1AMR2prev(NAMR2),p2AMR2prev(NAMR2),xAMR2prev(NAMR2),rAMR2prev(NAMR2),&
 fAMR2prev(NAMR2),gAMR2prev(NAMR2),QAMR2prev(NAMR2),betaAMR2prev(NAMR2),&
 dAMR2next(NAMR2),q1AMR2next(NAMR2),q2AMR2next(NAMR2),yAMR2next(NAMR2),s1AMR2next(NAMR2),s2AMR2next(NAMR2),&
 hAMR2next(NAMR2),aAMR2next(NAMR2),p1AMR2next(NAMR2),p2AMR2next(NAMR2),xAMR2next(NAMR2),rAMR2next(NAMR2),&
 fAMR2next(NAMR2),gAMR2next(NAMR2),QAMR2next(NAMR2),betaAMR2next(NAMR2))

uAMR2=0.D0
licznikAMR2=1

!!!!!!! WARUNKI POCZATKOWE MODULU AMR NA OSI uAMR=0 - PRZEPISANIE WARTOSCI !!!!!!!
CALL WarunkiPoczatkoweAMR(poczatekAMR2,NAMR2,&
 dAMR1prev,q1AMR1prev,q2AMR1prev,yAMR1prev,s1AMR1prev,s2AMR1prev,hAMR1prev,aAMR1prev,p1AMR1prev,p2AMR1prev,xAMR1prev,rAMR1prev,&
 fAMR1prev,gAMR1prev,QAMR1prev,betaAMR1prev,&
 !OUT:
 dAMR2prev,q1AMR2prev,q2AMR2prev,yAMR2prev,s1AMR2prev,s2AMR2prev,hAMR2prev,aAMR2prev,p1AMR2prev,p2AMR2prev,xAMR2prev,rAMR2prev,&
 fAMR2prev,gAMR2prev,QAMR2prev,betaAMR2prev)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! PETLA GLOWNA AMR2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO licznikAMR2=2,podzialAMR+1
   uAMR2=uAMR2+hAMR2
!  !!!!! WARUNKI BRZEGOWE MODULU AMR NA v(poczatekAMR) !!!!!
   IF (licznikAMR2.EQ.podzialAMR+1) THEN
      CALL WartosciFunkcjiAMRw1IF1(&
       dAMR1next(poczatekAMR2),q1AMR1next(poczatekAMR2),q2AMR1next(poczatekAMR2),yAMR1next(poczatekAMR2),&
       s1AMR1next(poczatekAMR2),s2AMR1next(poczatekAMR2),hAMR1next(poczatekAMR2),aAMR1next(poczatekAMR2),&
       p1AMR1next(poczatekAMR2),p2AMR1next(poczatekAMR2),xAMR1next(poczatekAMR2),rAMR1next(poczatekAMR2),fAMR1next(poczatekAMR2),&
       gAMR1next(poczatekAMR2),QAMR1next(poczatekAMR2),betaAMR1next(poczatekAMR2),&
       !OUT:
       dAMR2next(1),q1AMR2next(1),q2AMR2next(1),yAMR2next(1),s1AMR2next(1),s2AMR2next(1),hAMR2next(1),aAMR2next(1),&
       p1AMR2next(1),p2AMR2next(1),xAMR2next(1),rAMR2next(1),fAMR2next(1),gAMR2next(1),QAMR2next(1),betaAMR2next(1))
    ELSE
      IF (licznikAMR1.EQ.2) THEN
         CALL WartosciFunkcjiAMRw1IF2(podzialAMR,licznikAMR2,&
          s1AMR1prev(poczatekAMR2),s2AMR1prev(poczatekAMR2),hAMR1prev(poczatekAMR2),aAMR1prev(poczatekAMR2),&
          p1AMR1prev(poczatekAMR2),p2AMR1prev(poczatekAMR2),xAMR1prev(poczatekAMR2),fAMR1prev(poczatekAMR2),&
          QAMR1prev(poczatekAMR2),betaAMR1prev(poczatekAMR2),&
          s1AMR1next(poczatekAMR2),s2AMR1next(poczatekAMR2),hAMR1next(poczatekAMR2),aAMR1next(poczatekAMR2),&
          p1AMR1next(poczatekAMR2),p2AMR1next(poczatekAMR2),xAMR1next(poczatekAMR2),fAMR1next(poczatekAMR2),&
          QAMR1next(poczatekAMR2),betaAMR1next(poczatekAMR2),&
          !OUT:
          s1AMR2next(1),s2AMR2next(1),hAMR2next(1),aAMR2next(1),p1AMR2next(1),p2AMR2next(1),xAMR2next(1),fAMR2next(1),&
          QAMR2next(1),betaAMR2next(1))
    ELSE
      !  TWORZENIE TABLIC INTERPOLACYJNYCH CSI !
      CALL TworzenieTablicyFUNpppn(s1AMR1prev2(poczatekAMR2),s1AMR1prev(poczatekAMR2),s1AMR1next(poczatekAMR2),s1AMRCSI)
      CALL TworzenieTablicyFUNpppn(s2AMR1prev2(poczatekAMR2),s2AMR1prev(poczatekAMR2),s2AMR1next(poczatekAMR2),s2AMRCSI)
      CALL TworzenieTablicyFUNpppn(hAMR1prev2(poczatekAMR2),hAMR1prev(poczatekAMR2),hAMR1next(poczatekAMR2),hAMRCSI)
      CALL TworzenieTablicyFUNpppn(aAMR1prev2(poczatekAMR2),aAMR1prev(poczatekAMR2),aAMR1next(poczatekAMR2),aAMRCSI)
      CALL TworzenieTablicyFUNpppn(p1AMR1prev2(poczatekAMR2),p1AMR1prev(poczatekAMR2),p1AMR1next(poczatekAMR2),p1AMRCSI)
      CALL TworzenieTablicyFUNpppn(p2AMR1prev2(poczatekAMR2),p2AMR1prev(poczatekAMR2),p2AMR1next(poczatekAMR2),p2AMRCSI)
      CALL TworzenieTablicyFUNpppn(xAMR1prev2(poczatekAMR2),xAMR1prev(poczatekAMR2),xAMR1next(poczatekAMR2),xAMRCSI)
      CALL TworzenieTablicyFUNpppn(fAMR1prev2(poczatekAMR2),fAMR1prev(poczatekAMR2),fAMR1next(poczatekAMR2),fAMRCSI)
      CALL TworzenieTablicyFUNpppn(QAMR1prev2(poczatekAMR2),QAMR1prev(poczatekAMR2),QAMR1next(poczatekAMR2),QAMRCSI)
      CALL TworzenieTablicyFUNpppn(betaAMR1prev2(poczatekAMR2),betaAMR1prev(poczatekAMR2),betaAMR1next(poczatekAMR2),betaAMRCSI)
      !  INTERPOLACJA CSI !
      coef2=uAMR2*uAMR2
      coef3=coef2*uAMR2
      CALL CubicSplineInterpolationWSP(hAMR1,3,s1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s1AMR2next(1)=aCSI(2)+bCSI(2)*uAMR2+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR1,3,s2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s2AMR2next(1)=aCSI(2)+bCSI(2)*uAMR2+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR1,3,hAMRCSI,aCSI,bCSI,cCSI,dCSI)
      hAMR2next(1)=aCSI(2)+bCSI(2)*uAMR2+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR1,3,aAMRCSI,aCSI,bCSI,cCSI,dCSI)
      aAMR2next(1)=aCSI(2)+bCSI(2)*uAMR2+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR1,3,p1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p1AMR2next(1)=aCSI(2)+bCSI(2)*uAMR2+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR1,3,p2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p2AMR2next(1)=aCSI(2)+bCSI(2)*uAMR2+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR1,3,xAMRCSI,aCSI,bCSI,cCSI,dCSI)
      xAMR2next(1)=aCSI(2)+bCSI(2)*uAMR2+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR1,3,fAMRCSI,aCSI,bCSI,cCSI,dCSI)
      fAMR2next(1)=aCSI(2)+bCSI(2)*uAMR2+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR1,3,QAMRCSI,aCSI,bCSI,cCSI,dCSI)
      QAMR2next(1)=aCSI(2)+bCSI(2)*uAMR2+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR1,3,betaAMRCSI,aCSI,bCSI,cCSI,dCSI)
      betaAMR2next(1)=aCSI(2)+bCSI(2)*uAMR2+cCSI(2)*coef2+dCSI(2)*coef3
      END IF
   END IF

!  !!!!! OBLICZENIA WZDLUZ uAMR=const. !!!!!!!
   CALL EwolucjaU(h0,hAMR2,NAMR2,1,.TRUE.,licznikAMR1,&
    dAMR2prev,q1AMR2prev,q2AMR2prev,yAMR2prev,s1AMR2prev,s2AMR2prev,hAMR2prev,aAMR2prev,p1AMR2prev,p2AMR2prev,xAMR2prev,rAMR2prev,&
    fAMR2prev,gAMR2prev,QAMR2prev,betaAMR2prev,&
    s1AMR2prev2(1),s2AMR2prev2(1),hAMR2prev2(1),aAMR2prev2(1),p1AMR2prev2(1),p2AMR2prev2(1),xAMR2prev2(1),fAMR2prev2(1),&
    QAMR2prev2(1),betaAMR2prev2(1),&
    s1AMR2next(1),s2AMR2next(1),hAMR2next(1),aAMR2next(1),p1AMR2next(1),p2AMR2next(1),xAMR2next(1),fAMR2next(1),&
    QAMR2next(1),betaAMR2next(1),&
    !OUT:
    dAMR2next,q1AMR2next,q2AMR2next,yAMR2next)
   !
   IF (licznikAMR2.NE.podzialAMR+1) THEN
      CALL WarunkiBrzegowe(hAMR2,&
       rAMR2prev(1),gAMR2prev(1),hAMR2next(1),aAMR2next(1),fAMR2next(1),QAMR2next(1),&
       !OUT:
       rAMR2next(1),gAMR2next(1))
   END IF
   !
   CALL EwolucjaV(h0,NAMR2,1,&
    dAMR2next,q1AMR2next,q2AMR2next,yAMR2next,&
    !OUT:
    s1AMR2next,s2AMR2next,hAMR2next,aAMR2next,p1AMR2next,p2AMR2next,xAMR2next,rAMR2next,fAMR2next,gAMR2next,QAMR2next,betaAMR2next)

!  *!*!*!* KONTROLA BLEDU WZDLUZ uAMR=const. *!*!*!*
DO iAMR2=2,NAMR2
   funkcjableduAMR=ABS((rAMR2next(iAMR2)-rAMR2prev(iAMR2))/rAMR2prev(iAMR2))
   IF (funkcjableduAMR.GE.bladAMR) THEN
      poziomAMR=3
      poczatekAMR3=iAMR2-1
      CALL ZnajdzKoniecAMR(NAMR2,bladAMR,rAMR2prev,rAMR2next,poczatekAMR3,&
      !OUT:
      koniecAMR3)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! POZIOM AMR3 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
koniecAMR_minus_poczatekAMR3=koniecAMR3-poczatekAMR3
hAMR3=hAMR2/podzialAMR
NAMR3=koniecAMR_minus_poczatekAMR3+1

ALLOCATE(dAMR3prev2(NAMR3),q1AMR3prev2(NAMR3),q2AMR3prev2(NAMR3),yAMR3prev2(NAMR3),s1AMR3prev2(NAMR3),s2AMR3prev2(NAMR3),&
 hAMR3prev2(NAMR3),aAMR3prev2(NAMR3),p1AMR3prev2(NAMR3),p2AMR3prev2(NAMR3),xAMR3prev2(NAMR3),rAMR3prev2(NAMR3),&
 fAMR3prev2(NAMR3),gAMR3prev2(NAMR3),QAMR3prev2(NAMR3),betaAMR3prev2(NAMR3),&
 dAMR3prev(NAMR3),q1AMR3prev(NAMR3),q2AMR3prev(NAMR3),yAMR3prev(NAMR3),s1AMR3prev(NAMR3),s2AMR3prev(NAMR3),&
 hAMR3prev(NAMR3),aAMR3prev(NAMR3),p1AMR3prev(NAMR3),p2AMR3prev(NAMR3),xAMR3prev(NAMR3),rAMR3prev(NAMR3),&
 fAMR3prev(NAMR3),gAMR3prev(NAMR3),QAMR3prev(NAMR3),betaAMR3prev(NAMR3),&
 dAMR3next(NAMR3),q1AMR3next(NAMR3),q2AMR3next(NAMR3),yAMR3next(NAMR3),s1AMR3next(NAMR3),s2AMR3next(NAMR3),&
 hAMR3next(NAMR3),aAMR3next(NAMR3),p1AMR3next(NAMR3),p2AMR3next(NAMR3),xAMR3next(NAMR3),rAMR3next(NAMR3),&
 fAMR3next(NAMR3),gAMR3next(NAMR3),QAMR3next(NAMR3),betaAMR3next(NAMR3))

uAMR3=0.D0
licznikAMR3=1

!!!!!!! WARUNKI POCZATKOWE MODULU AMR NA OSI uAMR=0 - PRZEPISANIE WARTOSCI !!!!!!!
CALL WarunkiPoczatkoweAMR(poczatekAMR3,NAMR3,&
 dAMR2prev,q1AMR2prev,q2AMR2prev,yAMR2prev,s1AMR2prev,s2AMR2prev,hAMR2prev,aAMR2prev,p1AMR2prev,p2AMR2prev,xAMR2prev,rAMR2prev,&
 fAMR2prev,gAMR2prev,QAMR2prev,betaAMR2prev,&
 !OUT:
 dAMR3prev,q1AMR3prev,q2AMR3prev,yAMR3prev,s1AMR3prev,s2AMR3prev,hAMR3prev,aAMR3prev,p1AMR3prev,p2AMR3prev,xAMR3prev,rAMR3prev,&
 fAMR3prev,gAMR3prev,QAMR3prev,betaAMR3prev)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! PETLA GLOWNA AMR3 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO licznikAMR3=2,podzialAMR+1
   uAMR3=uAMR3+hAMR3
!  !!!!! WARUNKI BRZEGOWE MODULU AMR NA v(poczatekAMR) !!!!!
   IF (licznikAMR3.EQ.podzialAMR+1) THEN
      CALL WartosciFunkcjiAMRw1IF1(&
       dAMR2next(poczatekAMR3),q1AMR2next(poczatekAMR3),q2AMR2next(poczatekAMR3),yAMR2next(poczatekAMR3),&
       s1AMR2next(poczatekAMR3),s2AMR2next(poczatekAMR3),hAMR2next(poczatekAMR3),aAMR2next(poczatekAMR3),&
       p1AMR2next(poczatekAMR3),p2AMR2next(poczatekAMR3),xAMR2next(poczatekAMR3),rAMR2next(poczatekAMR3),fAMR2next(poczatekAMR3),&
       gAMR2next(poczatekAMR3),QAMR2next(poczatekAMR3),betaAMR2next(poczatekAMR3),&
       !OUT:
       dAMR3next(1),q1AMR3next(1),q2AMR3next(1),yAMR3next(1),s1AMR3next(1),s2AMR3next(1),hAMR3next(1),aAMR3next(1),&
       p1AMR3next(1),p2AMR3next(1),xAMR3next(1),rAMR3next(1),fAMR3next(1),gAMR3next(1),&
       QAMR3next(1),betaAMR3next(1))
    ELSE
      IF (licznikAMR2.EQ.2) THEN
         CALL WartosciFunkcjiAMRw1IF2(podzialAMR,licznikAMR3,&
          s1AMR2prev(poczatekAMR3),s2AMR2prev(poczatekAMR3),hAMR2prev(poczatekAMR3),aAMR2prev(poczatekAMR3),&
          p1AMR2prev(poczatekAMR3),p2AMR2prev(poczatekAMR3),xAMR2prev(poczatekAMR3),fAMR2prev(poczatekAMR3),&
          QAMR2prev(poczatekAMR3),betaAMR2prev(poczatekAMR3),&
          s1AMR2next(poczatekAMR3),s2AMR2next(poczatekAMR3),hAMR2next(poczatekAMR3),aAMR2next(poczatekAMR3),&
          p1AMR2next(poczatekAMR3),p2AMR2next(poczatekAMR3),xAMR2next(poczatekAMR3),fAMR2next(poczatekAMR3),&
          QAMR2next(poczatekAMR3),betaAMR2next(poczatekAMR3),&
          !OUT:
          s1AMR3next(1),s2AMR3next(1),hAMR3next(1),aAMR3next(1),p1AMR3next(1),p2AMR3next(1),xAMR3next(1),fAMR3next(1),&
          QAMR3next(1),betaAMR3next(1))
    ELSE
      !  TWORZENIE TABLIC INTERPOLACYJNYCH CSI !
      CALL TworzenieTablicyFUNpppn(s1AMR2prev2(poczatekAMR3),s1AMR2prev(poczatekAMR3),s1AMR2next(poczatekAMR3),s1AMRCSI)
      CALL TworzenieTablicyFUNpppn(s2AMR2prev2(poczatekAMR3),s2AMR2prev(poczatekAMR3),s2AMR2next(poczatekAMR3),s2AMRCSI)
      CALL TworzenieTablicyFUNpppn(hAMR2prev2(poczatekAMR3),hAMR2prev(poczatekAMR3),hAMR2next(poczatekAMR3),hAMRCSI)
      CALL TworzenieTablicyFUNpppn(aAMR2prev2(poczatekAMR3),aAMR2prev(poczatekAMR3),aAMR2next(poczatekAMR3),aAMRCSI)
      CALL TworzenieTablicyFUNpppn(p1AMR2prev2(poczatekAMR3),p1AMR2prev(poczatekAMR3),p1AMR2next(poczatekAMR3),p1AMRCSI)
      CALL TworzenieTablicyFUNpppn(p2AMR2prev2(poczatekAMR3),p2AMR2prev(poczatekAMR3),p2AMR2next(poczatekAMR3),p2AMRCSI)
      CALL TworzenieTablicyFUNpppn(xAMR2prev2(poczatekAMR3),xAMR2prev(poczatekAMR3),xAMR2next(poczatekAMR3),xAMRCSI)
      CALL TworzenieTablicyFUNpppn(fAMR2prev2(poczatekAMR3),fAMR2prev(poczatekAMR3),fAMR2next(poczatekAMR3),fAMRCSI)
      CALL TworzenieTablicyFUNpppn(QAMR2prev2(poczatekAMR3),QAMR2prev(poczatekAMR3),QAMR2next(poczatekAMR3),QAMRCSI)
      CALL TworzenieTablicyFUNpppn(betaAMR2prev2(poczatekAMR3),betaAMR2prev(poczatekAMR3),betaAMR2next(poczatekAMR3),betaAMRCSI)
      !  INTERPOLACJA CSI !
      coef2=uAMR3*uAMR3
      coef3=coef2*uAMR3
      CALL CubicSplineInterpolationWSP(hAMR2,3,s1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s1AMR3next(1)=aCSI(2)+bCSI(2)*uAMR3+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR2,3,s2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s2AMR3next(1)=aCSI(2)+bCSI(2)*uAMR3+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR2,3,hAMRCSI,aCSI,bCSI,cCSI,dCSI)
      hAMR3next(1)=aCSI(2)+bCSI(2)*uAMR3+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR2,3,aAMRCSI,aCSI,bCSI,cCSI,dCSI)
      aAMR3next(1)=aCSI(2)+bCSI(2)*uAMR3+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR2,3,p1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p1AMR3next(1)=aCSI(2)+bCSI(2)*uAMR3+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR2,3,p2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p2AMR3next(1)=aCSI(2)+bCSI(2)*uAMR3+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR2,3,xAMRCSI,aCSI,bCSI,cCSI,dCSI)
      xAMR3next(1)=aCSI(2)+bCSI(2)*uAMR3+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR2,3,fAMRCSI,aCSI,bCSI,cCSI,dCSI)
      fAMR3next(1)=aCSI(2)+bCSI(2)*uAMR3+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR2,3,QAMRCSI,aCSI,bCSI,cCSI,dCSI)
      QAMR3next(1)=aCSI(2)+bCSI(2)*uAMR3+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR2,3,betaAMRCSI,aCSI,bCSI,cCSI,dCSI)
      betaAMR3next(1)=aCSI(2)+bCSI(2)*uAMR3+cCSI(2)*coef2+dCSI(2)*coef3
      END IF
   END IF

!  !!!!! OBLICZENIA WZDLUZ uAMR=const. !!!!!!!
   CALL EwolucjaU(h0,hAMR3,NAMR3,1,.TRUE.,licznikAMR2,&
    dAMR3prev,q1AMR3prev,q2AMR3prev,yAMR3prev,s1AMR3prev,s2AMR3prev,hAMR3prev,aAMR3prev,p1AMR3prev,p2AMR3prev,xAMR3prev,rAMR3prev,&
    fAMR3prev,gAMR3prev,QAMR3prev,betaAMR3prev,&
    s1AMR3prev2(1),s2AMR3prev2(1),hAMR3prev2(1),aAMR3prev2(1),p1AMR3prev2(1),p2AMR3prev2(1),xAMR3prev2(1),fAMR3prev2(1),&
    QAMR3prev2(1),betaAMR3prev2(1),&
    s1AMR3next(1),s2AMR3next(1),hAMR3next(1),aAMR3next(1),p1AMR3next(1),p2AMR3next(1),xAMR3next(1),fAMR3next(1),&
    QAMR3next(1),betaAMR3next(1),&
    !OUT:
    dAMR3next,q1AMR3next,q2AMR3next,yAMR3next)
   !
   IF (licznikAMR3.NE.podzialAMR+1) THEN
      CALL WarunkiBrzegowe(hAMR3,&
       rAMR3prev(1),gAMR3prev(1),hAMR3next(1),aAMR3next(1),fAMR3next(1),QAMR3next(1),&
       !OUT:
       rAMR3next(1),gAMR3next(1))
   END IF
   !
   CALL EwolucjaV(h0,NAMR3,1,&
    dAMR3next,q1AMR3next,q2AMR3next,yAMR3next,&
    !OUT:
    s1AMR3next,s2AMR3next,hAMR3next,aAMR3next,p1AMR3next,p2AMR3next,xAMR3next,rAMR3next,fAMR3next,gAMR3next,QAMR3next,betaAMR3next)

!  *!*!*!* KONTROLA BLEDU WZDLUZ uAMR=const. *!*!*!*
DO iAMR3=2,NAMR3
   funkcjableduAMR=ABS((rAMR3next(iAMR3)-rAMR3prev(iAMR3))/rAMR3prev(iAMR3))
   IF (funkcjableduAMR.GE.bladAMR) THEN
      poziomAMR=4
      poczatekAMR4=iAMR3-1
      CALL ZnajdzKoniecAMR(NAMR3,bladAMR,rAMR3prev,rAMR3next,poczatekAMR4,&
      !OUT:
      koniecAMR4)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! POZIOM AMR4 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
koniecAMR_minus_poczatekAMR4=koniecAMR4-poczatekAMR4
hAMR4=hAMR3/podzialAMR
NAMR4=koniecAMR_minus_poczatekAMR4+1

ALLOCATE(dAMR4prev2(NAMR4),q1AMR4prev2(NAMR4),q2AMR4prev2(NAMR4),yAMR4prev2(NAMR4),s1AMR4prev2(NAMR4),s2AMR4prev2(NAMR4),&
 hAMR4prev2(NAMR4),aAMR4prev2(NAMR4),p1AMR4prev2(NAMR4),p2AMR4prev2(NAMR4),xAMR4prev2(NAMR4),rAMR4prev2(NAMR4),&
 fAMR4prev2(NAMR4),gAMR4prev2(NAMR4),QAMR4prev2(NAMR4),betaAMR4prev2(NAMR4),&
 dAMR4prev(NAMR4),q1AMR4prev(NAMR4),q2AMR4prev(NAMR4),yAMR4prev(NAMR4),s1AMR4prev(NAMR4),s2AMR4prev(NAMR4),&
 hAMR4prev(NAMR4),aAMR4prev(NAMR4),p1AMR4prev(NAMR4),p2AMR4prev(NAMR4),xAMR4prev(NAMR4),rAMR4prev(NAMR4),&
 fAMR4prev(NAMR4),gAMR4prev(NAMR4),QAMR4prev(NAMR4),betaAMR4prev(NAMR4),&
 dAMR4next(NAMR4),q1AMR4next(NAMR4),q2AMR4next(NAMR4),yAMR4next(NAMR4),s1AMR4next(NAMR4),s2AMR4next(NAMR4),&
 hAMR4next(NAMR4),aAMR4next(NAMR4),p1AMR4next(NAMR4),p2AMR4next(NAMR4),xAMR4next(NAMR4),rAMR4next(NAMR4),&
 fAMR4next(NAMR4),gAMR4next(NAMR4),QAMR4next(NAMR4),betaAMR4next(NAMR4))

uAMR4=0.D0
licznikAMR4=1

!!!!!!! WARUNKI POCZATKOWE MODULU AMR NA OSI uAMR=0 - PRZEPISANIE WARTOSCI !!!!!!!
CALL WarunkiPoczatkoweAMR(poczatekAMR4,NAMR4,&
 dAMR3prev,q1AMR3prev,q2AMR3prev,yAMR3prev,s1AMR3prev,s2AMR3prev,hAMR3prev,aAMR3prev,p1AMR3prev,p2AMR3prev,xAMR3prev,rAMR3prev,&
 fAMR3prev,gAMR3prev,QAMR3prev,betaAMR3prev,&
 !OUT:
 dAMR4prev,q1AMR4prev,q2AMR4prev,yAMR4prev,s1AMR4prev,s2AMR4prev,hAMR4prev,aAMR4prev,p1AMR4prev,p2AMR4prev,xAMR4prev,rAMR4prev,&
 fAMR4prev,gAMR4prev,QAMR4prev,betaAMR4prev)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! PETLA GLOWNA AMR4 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO licznikAMR4=2,podzialAMR+1
   uAMR4=uAMR4+hAMR4
!  !!!!! WARUNKI BRZEGOWE MODULU AMR NA v(poczatekAMR) !!!!!
   IF (licznikAMR4.EQ.podzialAMR+1) THEN
      CALL WartosciFunkcjiAMRw1IF1(&
       dAMR3next(poczatekAMR4),q1AMR3next(poczatekAMR4),q2AMR3next(poczatekAMR4),yAMR3next(poczatekAMR4),&
       s1AMR3next(poczatekAMR4),s2AMR3next(poczatekAMR4),hAMR3next(poczatekAMR4),aAMR3next(poczatekAMR4),&
       p1AMR3next(poczatekAMR4),p2AMR3next(poczatekAMR4),xAMR3next(poczatekAMR4),rAMR3next(poczatekAMR4),fAMR3next(poczatekAMR4),&
       gAMR3next(poczatekAMR4),QAMR3next(poczatekAMR4),betaAMR3next(poczatekAMR4),&
       !OUT:
       dAMR4next(1),q1AMR4next(1),q2AMR4next(1),yAMR4next(1),s1AMR4next(1),s2AMR4next(1),hAMR4next(1),aAMR4next(1),&
       p1AMR4next(1),p2AMR4next(1),xAMR4next(1),rAMR4next(1),fAMR4next(1),gAMR4next(1),QAMR4next(1),betaAMR4next(1))
    ELSE
      IF (licznikAMR3.EQ.2) THEN
         CALL WartosciFunkcjiAMRw1IF2(podzialAMR,licznikAMR4,&
          s1AMR3prev(poczatekAMR4),s2AMR3prev(poczatekAMR4),hAMR3prev(poczatekAMR4),aAMR3prev(poczatekAMR4),&
          p1AMR3prev(poczatekAMR4),p2AMR3prev(poczatekAMR4),xAMR3prev(poczatekAMR4),fAMR3prev(poczatekAMR4),&
          QAMR3prev(poczatekAMR4),betaAMR3prev(poczatekAMR4),&
          s1AMR3next(poczatekAMR4),s2AMR3next(poczatekAMR4),hAMR3next(poczatekAMR4),aAMR3next(poczatekAMR4),&
          p1AMR3next(poczatekAMR4),p2AMR3next(poczatekAMR4),xAMR3next(poczatekAMR4),fAMR3next(poczatekAMR4),&
          QAMR3next(poczatekAMR4),betaAMR3next(poczatekAMR4),&
          !OUT:
          s1AMR4next(1),s2AMR4next(1),hAMR4next(1),aAMR4next(1),p1AMR4next(1),p2AMR4next(1),xAMR4next(1),fAMR4next(1),&
          QAMR4next(1),betaAMR4next(1))
    ELSE
      !  TWORZENIE TABLIC INTERPOLACYJNYCH CSI !
      CALL TworzenieTablicyFUNpppn(s1AMR3prev2(poczatekAMR4),s1AMR3prev(poczatekAMR4),s1AMR3next(poczatekAMR4),s1AMRCSI)
      CALL TworzenieTablicyFUNpppn(s2AMR3prev2(poczatekAMR4),s2AMR3prev(poczatekAMR4),s2AMR3next(poczatekAMR4),s2AMRCSI)
      CALL TworzenieTablicyFUNpppn(hAMR3prev2(poczatekAMR4),hAMR3prev(poczatekAMR4),hAMR3next(poczatekAMR4),hAMRCSI)
      CALL TworzenieTablicyFUNpppn(aAMR3prev2(poczatekAMR4),aAMR3prev(poczatekAMR4),aAMR3next(poczatekAMR4),aAMRCSI)
      CALL TworzenieTablicyFUNpppn(p1AMR3prev2(poczatekAMR4),p1AMR3prev(poczatekAMR4),p1AMR3next(poczatekAMR4),p1AMRCSI)
      CALL TworzenieTablicyFUNpppn(p2AMR3prev2(poczatekAMR4),p2AMR3prev(poczatekAMR4),p2AMR3next(poczatekAMR4),p2AMRCSI)
      CALL TworzenieTablicyFUNpppn(xAMR3prev2(poczatekAMR4),xAMR3prev(poczatekAMR4),xAMR3next(poczatekAMR4),xAMRCSI)
      CALL TworzenieTablicyFUNpppn(fAMR3prev2(poczatekAMR4),fAMR3prev(poczatekAMR4),fAMR3next(poczatekAMR4),fAMRCSI)
      CALL TworzenieTablicyFUNpppn(QAMR3prev2(poczatekAMR4),QAMR3prev(poczatekAMR4),QAMR3next(poczatekAMR4),QAMRCSI)
      CALL TworzenieTablicyFUNpppn(betaAMR3prev2(poczatekAMR4),betaAMR3prev(poczatekAMR4),betaAMR3next(poczatekAMR4),betaAMRCSI)
      !  INTERPOLACJA CSI !
      coef2=uAMR4*uAMR4
      coef3=coef2*uAMR4
      CALL CubicSplineInterpolationWSP(hAMR3,3,s1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s1AMR4next(1)=aCSI(2)+bCSI(2)*uAMR4+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR3,3,s2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s2AMR4next(1)=aCSI(2)+bCSI(2)*uAMR4+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR3,3,hAMRCSI,aCSI,bCSI,cCSI,dCSI)
      hAMR4next(1)=aCSI(2)+bCSI(2)*uAMR4+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR3,3,aAMRCSI,aCSI,bCSI,cCSI,dCSI)
      aAMR4next(1)=aCSI(2)+bCSI(2)*uAMR4+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR3,3,p1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p1AMR4next(1)=aCSI(2)+bCSI(2)*uAMR4+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR3,3,p2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p2AMR4next(1)=aCSI(2)+bCSI(2)*uAMR4+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR3,3,xAMRCSI,aCSI,bCSI,cCSI,dCSI)
      xAMR4next(1)=aCSI(2)+bCSI(2)*uAMR4+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR3,3,fAMRCSI,aCSI,bCSI,cCSI,dCSI)
      fAMR4next(1)=aCSI(2)+bCSI(2)*uAMR4+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR3,3,QAMRCSI,aCSI,bCSI,cCSI,dCSI)
      QAMR4next(1)=aCSI(2)+bCSI(2)*uAMR4+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR3,3,betaAMRCSI,aCSI,bCSI,cCSI,dCSI)
      betaAMR4next(1)=aCSI(2)+bCSI(2)*uAMR4+cCSI(2)*coef2+dCSI(2)*coef3
      END IF
   END IF

!  !!!!! OBLICZENIA WZDLUZ uAMR=const. !!!!!!!
   CALL EwolucjaU(h0,hAMR4,NAMR4,1,.TRUE.,licznikAMR3,&
    dAMR4prev,q1AMR4prev,q2AMR4prev,yAMR4prev,s1AMR4prev,s2AMR4prev,hAMR4prev,aAMR4prev,p1AMR4prev,p2AMR4prev,xAMR4prev,rAMR4prev,&
    fAMR4prev,gAMR4prev,QAMR4prev,betaAMR4prev,&
    s1AMR4prev2(1),s2AMR4prev2(1),hAMR4prev2(1),aAMR4prev2(1),p1AMR4prev2(1),p2AMR4prev2(1),xAMR4prev2(1),fAMR4prev2(1),&
    QAMR4prev2(1),betaAMR4prev2(1),&
    s1AMR4next(1),s2AMR4next(1),hAMR4next(1),aAMR4next(1),p1AMR4next(1),p2AMR4next(1),xAMR4next(1),fAMR4next(1),&
    QAMR4next(1),betaAMR4next(1),&
    !OUT:
    dAMR4next,q1AMR4next,q2AMR4next,yAMR4next)
   !
   IF (licznikAMR4.NE.podzialAMR+1) THEN
      CALL WarunkiBrzegowe(hAMR4,&
       rAMR4prev(1),gAMR4prev(1),hAMR4next(1),aAMR4next(1),fAMR4next(1),QAMR4next(1),&
       !OUT:
       rAMR4next(1),gAMR4next(1))
   END IF
   !
   CALL EwolucjaV(h0,NAMR4,1,&
    dAMR4next,q1AMR4next,q2AMR4next,yAMR4next,&
    !OUT:
    s1AMR4next,s2AMR4next,hAMR4next,aAMR4next,p1AMR4next,p2AMR4next,xAMR4next,rAMR4next,fAMR4next,gAMR4next,QAMR4next,betaAMR4next)

!  *!*!*!* KONTROLA BLEDU WZDLUZ uAMR=const. *!*!*!*
DO iAMR4=2,NAMR4
   funkcjableduAMR=ABS((rAMR4next(iAMR4)-rAMR4prev(iAMR4))/rAMR4prev(iAMR4))
   IF (funkcjableduAMR.GE.bladAMR) THEN
      poziomAMR=5
      poczatekAMR5=iAMR4-1
      CALL ZnajdzKoniecAMR(NAMR4,bladAMR,rAMR4prev,rAMR4next,poczatekAMR5,&
      !OUT:
      koniecAMR5)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! POZIOM AMR5 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
koniecAMR_minus_poczatekAMR5=koniecAMR5-poczatekAMR5
hAMR5=hAMR4/podzialAMR
NAMR5=koniecAMR_minus_poczatekAMR5+1

ALLOCATE(dAMR5prev2(NAMR5),q1AMR5prev2(NAMR5),q2AMR5prev2(NAMR5),yAMR5prev2(NAMR5),s1AMR5prev2(NAMR5),s2AMR5prev2(NAMR5),&
 hAMR5prev2(NAMR5),aAMR5prev2(NAMR5),p1AMR5prev2(NAMR5),p2AMR5prev2(NAMR5),xAMR5prev2(NAMR5),rAMR5prev2(NAMR5),&
 fAMR5prev2(NAMR5),gAMR5prev2(NAMR5),QAMR5prev2(NAMR5),betaAMR5prev2(NAMR5),&
 dAMR5prev(NAMR5),q1AMR5prev(NAMR5),q2AMR5prev(NAMR5),yAMR5prev(NAMR5),s1AMR5prev(NAMR5),s2AMR5prev(NAMR5),&
 hAMR5prev(NAMR5),aAMR5prev(NAMR5),p1AMR5prev(NAMR5),p2AMR5prev(NAMR5),xAMR5prev(NAMR5),rAMR5prev(NAMR5),&
 fAMR5prev(NAMR5),gAMR5prev(NAMR5),QAMR5prev(NAMR5),betaAMR5prev(NAMR5),&
 dAMR5next(NAMR5),q1AMR5next(NAMR5),q2AMR5next(NAMR5),yAMR5next(NAMR5),s1AMR5next(NAMR5),s2AMR5next(NAMR5),&
 hAMR5next(NAMR5),aAMR5next(NAMR5),p1AMR5next(NAMR5),p2AMR5next(NAMR5),xAMR5next(NAMR5),rAMR5next(NAMR5),&
 fAMR5next(NAMR5),gAMR5next(NAMR5),QAMR5next(NAMR5),betaAMR5next(NAMR5))

uAMR5=0.D0
licznikAMR5=1

!!!!!!! WARUNKI POCZATKOWE MODULU AMR NA OSI uAMR=0 - PRZEPISANIE WARTOSCI !!!!!!!
CALL WarunkiPoczatkoweAMR(poczatekAMR5,NAMR5,&
 dAMR4prev,q1AMR4prev,q2AMR4prev,yAMR4prev,s1AMR4prev,s2AMR4prev,hAMR4prev,aAMR4prev,p1AMR4prev,p2AMR4prev,xAMR4prev,rAMR4prev,&
 fAMR4prev,gAMR4prev,QAMR4prev,betaAMR4prev,&
 !OUT:
 dAMR5prev,q1AMR5prev,q2AMR5prev,yAMR5prev,s1AMR5prev,s2AMR5prev,hAMR5prev,aAMR5prev,p1AMR5prev,p2AMR5prev,xAMR5prev,rAMR5prev,&
 fAMR5prev,gAMR5prev,QAMR5prev,betaAMR5prev)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! PETLA GLOWNA AMR5 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO licznikAMR5=2,podzialAMR+1
   uAMR5=uAMR5+hAMR5
!  !!!!! WARUNKI BRZEGOWE MODULU AMR NA v(poczatekAMR) !!!!!
   IF (licznikAMR5.EQ.podzialAMR+1) THEN
      CALL WartosciFunkcjiAMRw1IF1(&
       dAMR4next(poczatekAMR5),q1AMR4next(poczatekAMR5),q2AMR4next(poczatekAMR5),yAMR4next(poczatekAMR5),&
       s1AMR4next(poczatekAMR5),s2AMR4next(poczatekAMR5),hAMR4next(poczatekAMR5),aAMR4next(poczatekAMR5),&
       p1AMR4next(poczatekAMR5),p2AMR4next(poczatekAMR5),xAMR4next(poczatekAMR5),rAMR4next(poczatekAMR5),fAMR4next(poczatekAMR5),&
       gAMR4next(poczatekAMR5),QAMR4next(poczatekAMR5),betaAMR4next(poczatekAMR5),&
       !OUT:
       dAMR5next(1),q1AMR5next(1),q2AMR5next(1),yAMR5next(1),s1AMR5next(1),s2AMR5next(1),hAMR5next(1),aAMR5next(1),&
       p1AMR5next(1),p2AMR5next(1),xAMR5next(1),rAMR5next(1),fAMR5next(1),gAMR5next(1),&
       QAMR5next(1),betaAMR5next(1))
    ELSE
      IF (licznikAMR4.EQ.2) THEN
         CALL WartosciFunkcjiAMRw1IF2(podzialAMR,licznikAMR5,&
          s1AMR4prev(poczatekAMR5),s2AMR4prev(poczatekAMR5),hAMR4prev(poczatekAMR5),aAMR4prev(poczatekAMR5),&
          p1AMR4prev(poczatekAMR5),p2AMR4prev(poczatekAMR5),xAMR4prev(poczatekAMR5),fAMR4prev(poczatekAMR5),&
          QAMR4prev(poczatekAMR5),betaAMR4prev(poczatekAMR5),&
          s1AMR4next(poczatekAMR5),s2AMR4next(poczatekAMR5),hAMR4next(poczatekAMR5),aAMR4next(poczatekAMR5),&
          p1AMR4next(poczatekAMR5),p2AMR4next(poczatekAMR5),xAMR4next(poczatekAMR5),fAMR4next(poczatekAMR5),&
          QAMR4next(poczatekAMR5),betaAMR4next(poczatekAMR5),&
          !OUT:
          s1AMR5next(1),s2AMR5next(1),hAMR5next(1),aAMR5next(1),p1AMR5next(1),p2AMR5next(1),xAMR5next(1),fAMR5next(1),&
          QAMR5next(1),betaAMR5next(1))
    ELSE
      !  TWORZENIE TABLIC INTERPOLACYJNYCH CSI !
      CALL TworzenieTablicyFUNpppn(s1AMR4prev2(poczatekAMR5),s1AMR4prev(poczatekAMR5),s1AMR4next(poczatekAMR5),s1AMRCSI)
      CALL TworzenieTablicyFUNpppn(s2AMR4prev2(poczatekAMR5),s2AMR4prev(poczatekAMR5),s2AMR4next(poczatekAMR5),s2AMRCSI)
      CALL TworzenieTablicyFUNpppn(hAMR4prev2(poczatekAMR5),hAMR4prev(poczatekAMR5),hAMR4next(poczatekAMR5),hAMRCSI)
      CALL TworzenieTablicyFUNpppn(aAMR4prev2(poczatekAMR5),aAMR4prev(poczatekAMR5),aAMR4next(poczatekAMR5),aAMRCSI)
      CALL TworzenieTablicyFUNpppn(p1AMR4prev2(poczatekAMR5),p1AMR4prev(poczatekAMR5),p1AMR4next(poczatekAMR5),p1AMRCSI)
      CALL TworzenieTablicyFUNpppn(p2AMR4prev2(poczatekAMR5),p2AMR4prev(poczatekAMR5),p2AMR4next(poczatekAMR5),p2AMRCSI)
      CALL TworzenieTablicyFUNpppn(xAMR4prev2(poczatekAMR5),xAMR4prev(poczatekAMR5),xAMR4next(poczatekAMR5),xAMRCSI)
      CALL TworzenieTablicyFUNpppn(fAMR4prev2(poczatekAMR5),fAMR4prev(poczatekAMR5),fAMR4next(poczatekAMR5),fAMRCSI)
      CALL TworzenieTablicyFUNpppn(QAMR4prev2(poczatekAMR5),QAMR4prev(poczatekAMR5),QAMR4next(poczatekAMR5),QAMRCSI)
      CALL TworzenieTablicyFUNpppn(betaAMR4prev2(poczatekAMR5),betaAMR4prev(poczatekAMR5),betaAMR4next(poczatekAMR5),betaAMRCSI)
      !  INTERPOLACJA CSI !
      coef2=uAMR5*uAMR5
      coef3=coef2*uAMR5
      CALL CubicSplineInterpolationWSP(hAMR4,3,s1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s1AMR5next(1)=aCSI(2)+bCSI(2)*uAMR5+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR4,3,s2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s2AMR5next(1)=aCSI(2)+bCSI(2)*uAMR5+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR4,3,hAMRCSI,aCSI,bCSI,cCSI,dCSI)
      hAMR5next(1)=aCSI(2)+bCSI(2)*uAMR5+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR4,3,aAMRCSI,aCSI,bCSI,cCSI,dCSI)
      aAMR5next(1)=aCSI(2)+bCSI(2)*uAMR5+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR4,3,p1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p1AMR5next(1)=aCSI(2)+bCSI(2)*uAMR5+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR4,3,p2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p2AMR5next(1)=aCSI(2)+bCSI(2)*uAMR5+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR4,3,xAMRCSI,aCSI,bCSI,cCSI,dCSI)
      xAMR5next(1)=aCSI(2)+bCSI(2)*uAMR5+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR4,3,fAMRCSI,aCSI,bCSI,cCSI,dCSI)
      fAMR5next(1)=aCSI(2)+bCSI(2)*uAMR5+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR4,3,QAMRCSI,aCSI,bCSI,cCSI,dCSI)
      QAMR5next(1)=aCSI(2)+bCSI(2)*uAMR5+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(hAMR4,3,betaAMRCSI,aCSI,bCSI,cCSI,dCSI)
      betaAMR5next(1)=aCSI(2)+bCSI(2)*uAMR5+cCSI(2)*coef2+dCSI(2)*coef3
      END IF
   END IF

!  !!!!! OBLICZENIA WZDLUZ uAMR=const. !!!!!!!
   CALL EwolucjaU(h0,hAMR5,NAMR5,1,.TRUE.,licznikAMR4,&
    dAMR5prev,q1AMR5prev,q2AMR5prev,yAMR5prev,s1AMR5prev,s2AMR5prev,hAMR5prev,aAMR5prev,p1AMR5prev,p2AMR5prev,xAMR5prev,rAMR5prev,&
    fAMR5prev,gAMR5prev,QAMR5prev,betaAMR5prev,&
    s1AMR5prev2(1),s2AMR5prev2(1),hAMR5prev2(1),aAMR5prev2(1),p1AMR5prev2(1),p2AMR5prev2(1),xAMR5prev2(1),fAMR5prev2(1),&
    QAMR5prev2(1),betaAMR5prev2(1),&
    s1AMR5next(1),s2AMR5next(1),hAMR5next(1),aAMR5next(1),p1AMR5next(1),p2AMR5next(1),xAMR5next(1),fAMR5next(1),&
    QAMR5next(1),betaAMR5next(1),&
    !OUT:
    dAMR5next,q1AMR5next,q2AMR5next,yAMR5next)
   !
   IF (licznikAMR5.NE.podzialAMR+1) THEN
      CALL WarunkiBrzegowe(hAMR5,&
       rAMR5prev(1),gAMR5prev(1),hAMR5next(1),aAMR5next(1),fAMR5next(1),QAMR5next(1),&
       !OUT:
       rAMR5next(1),gAMR5next(1))
   END IF
   !
   CALL EwolucjaV(h0,NAMR5,1,&
    dAMR5next,q1AMR5next,q2AMR5next,yAMR5next,&
    !OUT:
    s1AMR5next,s2AMR5next,hAMR5next,aAMR5next,p1AMR5next,p2AMR5next,xAMR5next,rAMR5next,fAMR5next,gAMR5next,QAMR5next,betaAMR5next)

!  *!*!*!* KONTROLA BLEDU WZDLUZ uAMR=const. *!*!*!*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! DALSZE POZIOMY AMR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  !!!!! PRZEPISANIE WIELKOSCI OBLICZONYCH WZDLUZ POPRZEDNIEGO uAMR=const. DO PAMIECI PODRECZNEJ !!!!!
   CALL Przepisanie(&
    dAMR5next,yAMR5next,q1AMR5next,q2AMR5next,s1AMR5next,s2AMR5next,hAMR5next,aAMR5next,p1AMR5next,p2AMR5next,&
    xAMR5next,rAMR5next,fAMR5next,gAMR5next,QAMR5next,betaAMR5next,&
    !OUT:
    dAMR5prev,yAMR5prev,q1AMR5prev,q2AMR5prev,s1AMR5prev,s2AMR5prev,hAMR5prev,aAMR5prev,p1AMR5prev,p2AMR5prev,&
    xAMR5prev,rAMR5prev,fAMR5prev,gAMR5prev,QAMR5prev,betaAMR5prev,&
    dAMR5prev2,q1AMR5prev2,q2AMR5prev2,yAMR5prev2,s1AMR5prev2,s2AMR5prev2,hAMR5prev2,aAMR5prev2,p1AMR5prev2,p2AMR5prev2,&
    xAMR5prev2,rAMR5prev2,fAMR5prev2,gAMR5prev2,QAMR5prev2,betaAMR5prev2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC PETLI GLOWNEJ AMR5 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END DO

!!!!! PRZEPISANIE WARTOSCI DO RZADSZEJ SIATKI 5-->4 !!!!!
CALL PrzepisanieNaNizszyPoziomAMR(poczatekAMR5,NAMR5,&
 dAMR5next,yAMR5next,q1AMR5next,q2AMR5next,s1AMR5next,s2AMR5next,hAMR5next,aAMR5next,p1AMR5next,p2AMR5next,xAMR5next,rAMR5next,&
 fAMR5next,gAMR5next,QAMR5next,betaAMR5next,&
 !OUT:
 dAMR4next,yAMR4next,q1AMR4next,q2AMR4next,s1AMR4next,s2AMR4next,hAMR4next,aAMR4next,p1AMR4next,p2AMR4next,xAMR4next,rAMR4next,&
 fAMR4next,gAMR4next,QAMR4next,betaAMR4next)

DEALLOCATE(dAMR5prev2,q1AMR5prev2,q2AMR5prev2,yAMR5prev2,s1AMR5prev2,s2AMR5prev2,hAMR5prev2,aAMR5prev2,p1AMR5prev2,p2AMR5prev2)
DEALLOCATE(xAMR5prev2,rAMR5prev2,fAMR5prev2,gAMR5prev2,QAMR5prev2,betaAMR5prev2)
DEALLOCATE(dAMR5prev,q1AMR5prev,q2AMR5prev,yAMR5prev,s1AMR5prev,s2AMR5prev,hAMR5prev,aAMR5prev,p1AMR5prev,p2AMR5prev)
DEALLOCATE(xAMR5prev,rAMR5prev,fAMR5prev,gAMR5prev,QAMR5prev,betaAMR5prev)
DEALLOCATE(dAMR5next,q1AMR5next,q2AMR5next,yAMR5next,s1AMR5next,s2AMR5next,hAMR5next,aAMR5next,p1AMR5next,p2AMR5next)
DEALLOCATE(xAMR5next,rAMR5next,fAMR5next,gAMR5next,QAMR5next,betaAMR5next)

!!!!! DOLICZENIE WARTOSCI DO KONCA RZADSZEJ SIATKI !!!!!
IF (koniecAMR5.LT.NAMR4) THEN
   CALL EwolucjaU(h0,hAMR4,NAMR4,koniecAMR5,.TRUE.,licznikAMR4,&
    dAMR4prev,q1AMR4prev,q2AMR4prev,yAMR4prev,s1AMR4prev,s2AMR4prev,hAMR4prev,aAMR4prev,p1AMR4prev,p2AMR4prev,xAMR4prev,rAMR4prev,&
    fAMR4prev,gAMR4prev,QAMR4prev,betaAMR4prev,&
    s1AMR4prev2(koniecAMR5),s2AMR4prev2(koniecAMR5),hAMR4prev2(koniecAMR5),aAMR4prev2(koniecAMR5),&
    p1AMR4prev2(koniecAMR5),p2AMR4prev2(koniecAMR5),xAMR4prev2(koniecAMR5),fAMR4prev2(koniecAMR5),&
    QAMR4prev2(koniecAMR5),betaAMR4prev2(koniecAMR5),&
    s1AMR4next(koniecAMR5),s2AMR4next(koniecAMR5),hAMR4next(koniecAMR5),aAMR4next(koniecAMR5),&
    p1AMR4next(koniecAMR5),p2AMR4next(koniecAMR5),xAMR4next(koniecAMR5),fAMR4next(koniecAMR5),&
    QAMR4next(koniecAMR5),betaAMR4next(koniecAMR5),&
    !OUT:
    dAMR4next,q1AMR4next,q2AMR4next,yAMR4next)
   !
   CALL EwolucjaV(h0,NAMR4,koniecAMR5,&
    dAMR4next,q1AMR4next,q2AMR4next,yAMR4next,&
    !OUT:
    s1AMR4next,s2AMR4next,hAMR4next,aAMR4next,p1AMR4next,p2AMR4next,xAMR4next,rAMR4next,fAMR4next,gAMR4next,QAMR4next,betaAMR4next)
END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC POZIOMU AMR5 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      EXIT
   END IF
END DO

!  !!!!! PRZEPISANIE WIELKOSCI OBLICZONYCH WZDLUZ POPRZEDNIEGO uAMR=const. DO PAMIECI PODRECZNEJ !!!!!
   CALL Przepisanie(&
    dAMR4next,yAMR4next,q1AMR4next,q2AMR4next,s1AMR4next,s2AMR4next,hAMR4next,aAMR4next,p1AMR4next,p2AMR4next,&
    xAMR4next,rAMR4next,fAMR4next,gAMR4next,QAMR4next,betaAMR4next,&
    !OUT:
    dAMR4prev,yAMR4prev,q1AMR4prev,q2AMR4prev,s1AMR4prev,s2AMR4prev,hAMR4prev,aAMR4prev,p1AMR4prev,p2AMR4prev,&
    xAMR4prev,rAMR4prev,fAMR4prev,gAMR4prev,QAMR4prev,betaAMR4prev,&
    dAMR4prev2,q1AMR4prev2,q2AMR4prev2,yAMR4prev2,s1AMR4prev2,s2AMR4prev2,hAMR4prev2,aAMR4prev2,p1AMR4prev2,p2AMR4prev2,&
    xAMR4prev2,rAMR4prev2,fAMR4prev2,gAMR4prev2,QAMR4prev2,betaAMR4prev2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC PETLI GLOWNEJ AMR4 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END DO

!!!!! PRZEPISANIE WARTOSCI DO RZADSZEJ SIATKI 4-->3 !!!!!
CALL PrzepisanieNaNizszyPoziomAMR(poczatekAMR4,NAMR4,&
 dAMR4next,yAMR4next,q1AMR4next,q2AMR4next,s1AMR4next,s2AMR4next,hAMR4next,aAMR4next,p1AMR4next,p2AMR4next,xAMR4next,rAMR4next,&
 fAMR4next,gAMR4next,QAMR4next,betaAMR4next,&
 !OUT:
 dAMR3next,yAMR3next,q1AMR3next,q2AMR3next,s1AMR3next,s2AMR3next,hAMR3next,aAMR3next,p1AMR3next,p2AMR3next,xAMR3next,rAMR3next,&
 fAMR3next,gAMR3next,QAMR3next,betaAMR3next)

DEALLOCATE(dAMR4prev2,q1AMR4prev2,q2AMR4prev2,yAMR4prev2,s1AMR4prev2,s2AMR4prev2,hAMR4prev2,aAMR4prev2,p1AMR4prev2,p2AMR4prev2)
DEALLOCATE(xAMR4prev2,rAMR4prev2,fAMR4prev2,gAMR4prev2,QAMR4prev2,betaAMR4prev2)
DEALLOCATE(dAMR4prev,q1AMR4prev,q2AMR4prev,yAMR4prev,s1AMR4prev,s2AMR4prev,hAMR4prev,aAMR4prev,p1AMR4prev,p2AMR4prev)
DEALLOCATE(xAMR4prev,rAMR4prev,fAMR4prev,gAMR4prev,QAMR4prev,betaAMR4prev)
DEALLOCATE(dAMR4next,q1AMR4next,q2AMR4next,yAMR4next,s1AMR4next,s2AMR4next,hAMR4next,aAMR4next,p1AMR4next,p2AMR4next)
DEALLOCATE(xAMR4next,rAMR4next,fAMR4next,gAMR4next,QAMR4next,betaAMR4next)

!!!!! DOLICZENIE WARTOSCI DO KONCA RZADSZEJ SIATKI !!!!!
IF (koniecAMR4.LT.NAMR3) THEN
   CALL EwolucjaU(h0,hAMR3,NAMR3,koniecAMR4,.TRUE.,licznikAMR3,&
    dAMR3prev,q1AMR3prev,q2AMR3prev,yAMR3prev,s1AMR3prev,s2AMR3prev,hAMR3prev,aAMR3prev,p1AMR3prev,p2AMR3prev,xAMR3prev,rAMR3prev,&
    fAMR3prev,gAMR3prev,QAMR3prev,betaAMR3prev,&
    s1AMR3prev2(koniecAMR4),s2AMR3prev2(koniecAMR4),hAMR3prev2(koniecAMR4),aAMR3prev2(koniecAMR4),&
    p1AMR3prev2(koniecAMR4),p2AMR3prev2(koniecAMR4),xAMR3prev2(koniecAMR4),fAMR3prev2(koniecAMR4),&
    QAMR3prev2(koniecAMR4),betaAMR3prev2(koniecAMR4),&
    s1AMR3next(koniecAMR4),s2AMR3next(koniecAMR4),hAMR3next(koniecAMR4),aAMR3next(koniecAMR4),&
    p1AMR3next(koniecAMR4),p2AMR3next(koniecAMR4),xAMR3next(koniecAMR4),fAMR3next(koniecAMR4),&
    QAMR3next(koniecAMR4),betaAMR3next(koniecAMR4),&
    !OUT:
    dAMR3next,q1AMR3next,q2AMR3next,yAMR3next)
   !
   CALL EwolucjaV(h0,NAMR3,koniecAMR4,&
    dAMR3next,q1AMR3next,q2AMR3next,yAMR3next,&
    !OUT:
    s1AMR3next,s2AMR3next,hAMR3next,aAMR3next,p1AMR3next,p2AMR3next,xAMR3next,rAMR3next,fAMR3next,gAMR3next,QAMR3next,betaAMR3next)
END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC POZIOMU AMR4 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      EXIT
   END IF
END DO

!  !!!!! PRZEPISANIE WIELKOSCI OBLICZONYCH WZDLUZ POPRZEDNIEGO uAMR=const. DO PAMIECI PODRECZNEJ !!!!!
   CALL Przepisanie(&
    dAMR3next,yAMR3next,q1AMR3next,q2AMR3next,s1AMR3next,s2AMR3next,hAMR3next,aAMR3next,p1AMR3next,p2AMR3next,&
    xAMR3next,rAMR3next,fAMR3next,gAMR3next,QAMR3next,betaAMR3next,&
    !OUT:
    dAMR3prev,yAMR3prev,q1AMR3prev,q2AMR3prev,s1AMR3prev,s2AMR3prev,hAMR3prev,aAMR3prev,p1AMR3prev,p2AMR3prev,&
    xAMR3prev,rAMR3prev,fAMR3prev,gAMR3prev,QAMR3prev,betaAMR3prev,&
    dAMR3prev2,q1AMR3prev2,q2AMR3prev2,yAMR3prev2,s1AMR3prev2,s2AMR3prev2,hAMR3prev2,aAMR3prev2,p1AMR3prev2,p2AMR3prev2,&
    xAMR3prev2,rAMR3prev2,fAMR3prev2,gAMR3prev2,QAMR3prev2,betaAMR3prev2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC PETLI GLOWNEJ AMR3 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END DO

!!!!! PRZEPISANIE WARTOSCI DO RZADSZEJ SIATKI 3-->2 !!!!!
CALL PrzepisanieNaNizszyPoziomAMR(poczatekAMR3,NAMR3,&
 dAMR3next,yAMR3next,q1AMR3next,q2AMR3next,s1AMR3next,s2AMR3next,hAMR3next,aAMR3next,p1AMR3next,p2AMR3next,xAMR3next,rAMR3next,&
 fAMR3next,gAMR3next,QAMR3next,betaAMR3next,&
 !OUT:
 dAMR2next,yAMR2next,q1AMR2next,q2AMR2next,s1AMR2next,s2AMR2next,hAMR2next,aAMR2next,p1AMR2next,p2AMR2next,xAMR2next,rAMR2next,&
 fAMR2next,gAMR2next,QAMR2next,betaAMR2next)

DEALLOCATE(dAMR3prev2,q1AMR3prev2,q2AMR3prev2,yAMR3prev2,s1AMR3prev2,s2AMR3prev2,hAMR3prev2,aAMR3prev2,p1AMR3prev2,p2AMR3prev2)
DEALLOCATE(xAMR3prev2,rAMR3prev2,fAMR3prev2,gAMR3prev2,QAMR3prev2,betaAMR3prev2)
DEALLOCATE(dAMR3prev,q1AMR3prev,q2AMR3prev,yAMR3prev,s1AMR3prev,s2AMR3prev,hAMR3prev,aAMR3prev,p1AMR3prev,p2AMR3prev)
DEALLOCATE(xAMR3prev,rAMR3prev,fAMR3prev,gAMR3prev,QAMR3prev,betaAMR3prev)
DEALLOCATE(dAMR3next,q1AMR3next,q2AMR3next,yAMR3next,s1AMR3next,s2AMR3next,hAMR3next,aAMR3next,p1AMR3next,p2AMR3next)
DEALLOCATE(xAMR3next,rAMR3next,fAMR3next,gAMR3next,QAMR3next,betaAMR3next)

!!!!! DOLICZENIE WARTOSCI DO KONCA RZADSZEJ SIATKI !!!!!
IF (koniecAMR3.LT.NAMR2) THEN
   CALL EwolucjaU(h0,hAMR2,NAMR2,koniecAMR3,.TRUE.,licznikAMR2,&
    dAMR2prev,q1AMR2prev,q2AMR2prev,yAMR2prev,s1AMR2prev,s2AMR2prev,hAMR2prev,aAMR2prev,p1AMR2prev,p2AMR2prev,xAMR2prev,rAMR2prev,&
    fAMR2prev,gAMR2prev,QAMR2prev,betaAMR2prev,&
    s1AMR2prev2(koniecAMR3),s2AMR2prev2(koniecAMR3),hAMR2prev2(koniecAMR3),aAMR2prev2(koniecAMR3),&
    p1AMR2prev2(koniecAMR3),p2AMR2prev2(koniecAMR3),xAMR2prev2(koniecAMR3),fAMR2prev2(koniecAMR3),&
    QAMR2prev2(koniecAMR3),betaAMR2prev2(koniecAMR3),&
    s1AMR2next(koniecAMR3),s2AMR2next(koniecAMR3),hAMR2next(koniecAMR3),aAMR2next(koniecAMR3),&
    p1AMR2next(koniecAMR3),p2AMR2next(koniecAMR3),xAMR2next(koniecAMR3),fAMR2next(koniecAMR3),&
    QAMR2next(koniecAMR3),betaAMR2next(koniecAMR3),&
    !OUT:
    dAMR2next,q1AMR2next,q2AMR2next,yAMR2next)
   !
   CALL EwolucjaV(h0,NAMR2,koniecAMR3,&
    dAMR2next,q1AMR2next,q2AMR2next,yAMR2next,&
    !OUT:
    s1AMR2next,s2AMR2next,hAMR2next,aAMR2next,p1AMR2next,p2AMR2next,xAMR2next,rAMR2next,fAMR2next,gAMR2next,QAMR2next,betaAMR2next)
END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC POZIOMU AMR3 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      EXIT
   END IF
END DO

!  !!!!! PRZEPISANIE WIELKOSCI OBLICZONYCH WZDLUZ POPRZEDNIEGO uAMR=const. DO PAMIECI PODRECZNEJ !!!!!
   CALL Przepisanie(&
    dAMR2next,yAMR2next,q1AMR2next,q2AMR2next,s1AMR2next,s2AMR2next,hAMR2next,aAMR2next,p1AMR2next,p2AMR2next,&
    xAMR2next,rAMR2next,fAMR2next,gAMR2next,QAMR2next,betaAMR2next,&
    !OUT:
    dAMR2prev,yAMR2prev,q1AMR2prev,q2AMR2prev,s1AMR2prev,s2AMR2prev,hAMR2prev,aAMR2prev,p1AMR2prev,p2AMR2prev,&
    xAMR2prev,rAMR2prev,fAMR2prev,gAMR2prev,QAMR2prev,betaAMR2prev,&
    dAMR2prev2,q1AMR2prev2,q2AMR2prev2,yAMR2prev2,s1AMR2prev2,s2AMR2prev2,hAMR2prev2,aAMR2prev2,p1AMR2prev2,p2AMR2prev2,&
    xAMR2prev2,rAMR2prev2,fAMR2prev2,gAMR2prev2,QAMR2prev2,betaAMR2prev2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC PETLI GLOWNEJ AMR2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END DO

!!!!! PRZEPISANIE WARTOSCI DO RZADSZEJ SIATKI 2-->1 !!!!!
CALL PrzepisanieNaNizszyPoziomAMR(poczatekAMR2,NAMR2,&
 dAMR2next,yAMR2next,q1AMR2next,q2AMR2next,s1AMR2next,s2AMR2next,hAMR2next,aAMR2next,p1AMR2next,p2AMR2next,xAMR2next,rAMR2next,&
 fAMR2next,gAMR2next,QAMR2next,betaAMR2next,&
 !OUT:
 dAMR1next,yAMR1next,q1AMR1next,q2AMR1next,s1AMR1next,s2AMR1next,hAMR1next,aAMR1next,p1AMR1next,p2AMR1next,xAMR1next,rAMR1next,&
 fAMR1next,gAMR1next,QAMR1next,betaAMR1next)

DEALLOCATE(dAMR2prev2,q1AMR2prev2,q2AMR2prev2,yAMR2prev2,s1AMR2prev2,s2AMR2prev2,hAMR2prev2,aAMR2prev2,p1AMR2prev2,p2AMR2prev2)
DEALLOCATE(xAMR2prev2,rAMR2prev2,fAMR2prev2,gAMR2prev2,QAMR2prev2,betaAMR2prev2)
DEALLOCATE(dAMR2prev,q1AMR2prev,q2AMR2prev,yAMR2prev,s1AMR2prev,s2AMR2prev,hAMR2prev,aAMR2prev,p1AMR2prev,p2AMR2prev)
DEALLOCATE(xAMR2prev,rAMR2prev,fAMR2prev,gAMR2prev,QAMR2prev,betaAMR2prev)
DEALLOCATE(dAMR2next,q1AMR2next,q2AMR2next,yAMR2next,s1AMR2next,s2AMR2next,hAMR2next,aAMR2next,p1AMR2next,p2AMR2next)
DEALLOCATE(xAMR2next,rAMR2next,fAMR2next,gAMR2next,QAMR2next,betaAMR2next)

!!!!! DOLICZENIE WARTOSCI DO KONCA RZADSZEJ SIATKI !!!!!
IF (koniecAMR2.LT.NAMR1) THEN
   CALL EwolucjaU(h0,hAMR1,NAMR1,koniecAMR2,.TRUE.,licznikAMR1,&
    dAMR1prev,q1AMR1prev,q2AMR1prev,yAMR1prev,s1AMR1prev,s2AMR1prev,hAMR1prev,aAMR1prev,p1AMR1prev,p2AMR1prev,xAMR1prev,rAMR1prev,&
    fAMR1prev,gAMR1prev,QAMR1prev,betaAMR1prev,&
    s1AMR1prev2(koniecAMR2),s2AMR1prev2(koniecAMR2),hAMR1prev2(koniecAMR2),aAMR1prev2(koniecAMR2),&
    p1AMR1prev2(koniecAMR2),p2AMR1prev2(koniecAMR2),xAMR1prev2(koniecAMR2),fAMR1prev2(koniecAMR2),&
    QAMR1prev2(koniecAMR2),betaAMR1prev2(koniecAMR2),&
    s1AMR1next(koniecAMR2),s2AMR1next(koniecAMR2),hAMR1next(koniecAMR2),aAMR1next(koniecAMR2),&
    p1AMR1next(koniecAMR2),p2AMR1next(koniecAMR2),xAMR1next(koniecAMR2),fAMR1next(koniecAMR2),&
    QAMR1next(koniecAMR2),betaAMR1next(koniecAMR2),&
    !OUT:
    dAMR1next,q1AMR1next,q2AMR1next,yAMR1next)
   !
   CALL EwolucjaV(h0,NAMR1,koniecAMR2,&
    dAMR1next,q1AMR1next,q2AMR1next,yAMR1next,&
    !OUT:
    s1AMR1next,s2AMR1next,hAMR1next,aAMR1next,p1AMR1next,p2AMR1next,xAMR1next,rAMR1next,fAMR1next,gAMR1next,QAMR1next,betaAMR1next)
END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC POZIOMU AMR2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      EXIT
   END IF
END DO

!  !!!!! PRZEPISANIE WIELKOSCI OBLICZONYCH WZDLUZ POPRZEDNIEGO uAMR=const. DO PAMIECI PODRECZNEJ !!!!!
   CALL Przepisanie(&
    dAMR1next,yAMR1next,q1AMR1next,q2AMR1next,s1AMR1next,s2AMR1next,hAMR1next,aAMR1next,p1AMR1next,p2AMR1next,&
    xAMR1next,rAMR1next,fAMR1next,gAMR1next,QAMR1next,betaAMR1next,&
    !OUT:
    dAMR1prev,yAMR1prev,q1AMR1prev,q2AMR1prev,s1AMR1prev,s2AMR1prev,hAMR1prev,aAMR1prev,p1AMR1prev,p2AMR1prev,&
    xAMR1prev,rAMR1prev,fAMR1prev,gAMR1prev,QAMR1prev,betaAMR1prev,&
    dAMR1prev2,q1AMR1prev2,q2AMR1prev2,yAMR1prev2,s1AMR1prev2,s2AMR1prev2,hAMR1prev2,aAMR1prev2,p1AMR1prev2,p2AMR1prev2,&
    xAMR1prev2,rAMR1prev2,fAMR1prev2,gAMR1prev2,QAMR1prev2,betaAMR1prev2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC PETLI GLOWNEJ AMR1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END DO

!!!!! PRZEPISANIE WARTOSCI DO RZADSZEJ SIATKI 1-->0 !!!!!
CALL PrzepisanieNaNizszyPoziomAMR(poczatekAMR1,NAMR1,&
 dAMR1next,yAMR1next,q1AMR1next,q2AMR1next,s1AMR1next,s2AMR1next,hAMR1next,aAMR1next,p1AMR1next,p2AMR1next,xAMR1next,rAMR1next,&
 fAMR1next,gAMR1next,QAMR1next,betaAMR1next,&
 !OUT:
 dnext,ynext,q1next,q2next,s1next,s2next,hnext,anext,p1next,p2next,xnext,rnext,fnext,gnext,Qnext,betanext)

DEALLOCATE(dAMR1prev2,q1AMR1prev2,q2AMR1prev2,yAMR1prev2,s1AMR1prev2,s2AMR1prev2,hAMR1prev2,aAMR1prev2,p1AMR1prev2,p2AMR1prev2)
DEALLOCATE(xAMR1prev2,rAMR1prev2,fAMR1prev2,gAMR1prev2,QAMR1prev2,betaAMR1prev2)
DEALLOCATE(dAMR1prev,q1AMR1prev,q2AMR1prev,yAMR1prev,s1AMR1prev,s2AMR1prev,hAMR1prev,aAMR1prev,p1AMR1prev,p2AMR1prev)
DEALLOCATE(xAMR1prev,rAMR1prev,fAMR1prev,gAMR1prev,QAMR1prev,betaAMR1prev)
DEALLOCATE(dAMR1next,q1AMR1next,q2AMR1next,yAMR1next,s1AMR1next,s2AMR1next,hAMR1next,aAMR1next,p1AMR1next,p2AMR1next)
DEALLOCATE(xAMR1next,rAMR1next,fAMR1next,gAMR1next,QAMR1next,betaAMR1next)

!!!!! DOLICZENIE WARTOSCI DO KONCA RZADSZEJ SIATKI !!!!!
IF (koniecAMR1.LT.N) THEN
   CALL EwolucjaU(h0,h0,N,koniecAMR1,.TRUE.,licznik,&
    dprev,q1prev,q2prev,yprev,s1prev,s2prev,hprev,aprev,p1prev,p2prev,xprev,rprev,fprev,gprev,Qprev,betaprev,&
    s1prev2(koniecAMR1),s2prev2(koniecAMR1),hprev2(koniecAMR1),aprev2(koniecAMR1),&
    p1prev2(koniecAMR1),p2prev2(koniecAMR1),xprev2(koniecAMR1),fprev2(koniecAMR1),Qprev2(koniecAMR1),betaprev2(koniecAMR1),&
    s1next(koniecAMR1),s2next(koniecAMR1),hnext(koniecAMR1),anext(koniecAMR1),&
    p1next(koniecAMR1),p2next(koniecAMR1),xnext(koniecAMR1),fnext(koniecAMR1),Qnext(koniecAMR1),betanext(koniecAMR1),&
    !OUT:
    dnext,q1next,q2next,ynext)
   !
   CALL EwolucjaV(h0,N,koniecAMR1,&
    dnext,q1next,q2next,ynext,&
    !OUT:
    s1next,s2next,hnext,anext,p1next,p2next,xnext,rnext,fnext,gnext,Qnext,betanext)
END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC POZIOMU AMR1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      EXIT
   END IF
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! KONIEC SIATKI SAMOADAPTACYJNEJ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!******* wypisanie danych do plikow ***************************************************************
!DO i=1,Nkoniec
!   IF (rnext(i).NE.rnext(i)) THEN   !tylko NaN nie spelnia warunku
!      WRITE(2,30) u,v(i-2),rnext(i-2)
!      EXIT
!   END IF
!END DO
!
!DO i=1,Nkoniec
! DO j=1,linieRconst
!    R=DABS(rnext(i)-R4v(j))
!    IF (R.LE.Rconst) WRITE(3,30) u,v(i),rnext(i)
! END DO
!END DO
!
!DO i=1,Nkoniec
! DO j=1,3*linieRconst+1
!    R=DABS(rnext(i)-R4u(j))
!    IF (R.LE.Rconst) WRITE(6,30) u,v(i),rnext(i)
! END DO
!END DO
!
DO i=1,Nkoniec
   H=gprev(i)*gnext(i)
   IF (H.LE.0.D0) THEN
!      gPOv=(gnext(i)-gnext(i-1))/h0
!      gPOu=(gnext(i)-gprev(i))/(u-uprev)
      mEH=0.5D0*rnext(i)&
       *(1.D0+4.D0*fnext(i)*gnext(i)/(anext(i)*anext(i))+Qnext(i)*Qnext(i)/(rnext(i)*rnext(i)))
      WRITE(4,60) u,v(i),rnext(i),mEH,Qnext(i),&
       DerivAsymBackward2pts(h0,aprev(i),anext(i))*0.5D0/(anext(i)*anext(i)*pi)
!      WRITE(4,70) u,v(i),rnext(i),gPOv,gPOu,mEH,Qnext(i)
   END IF
END DO
!DO i=1,Nkoniec
!   H=fnext(i-1)*fnext(i)
!   IF (H.LE.0) THEN
!      fPOu=(fnext(i)-fprev(i))/(u-uprev)
!      fPOv=(fnext(i)-fnext(i-1))/h0
!      WRITE(5,30) u,v(i),rnext(i)
!      WRITE(5,50) u,v(i),rnext(i),fPOu,fPOv
!   END IF
!END DO


!******* PRZEPISANIE WIELKOSCI DO PAMIECI PODRECZNEJ *****
CALL Przepisanie(&
 dnext,ynext,q1next,q2next,s1next,s2next,hnext,anext,p1next,p2next,xnext,rnext,fnext,gnext,Qnext,betanext,&
 !OUT:
 dprev,yprev,q1prev,q2prev,s1prev,s2prev,hprev,aprev,p1prev,p2prev,xprev,rprev,fprev,gprev,Qprev,betaprev,&
 dprev2,q1prev2,q2prev2,yprev2,s1prev2,s2prev2,hprev2,aprev2,p1prev2,p2prev2,xprev2,rprev2,fprev2,gprev2,Qprev2,betaprev2)
!
uprev=u

!**************************************************************************************************
!****** KONIEC PETLI GLOWNEJ **********************************************************************
!**************************************************************************************************
END DO


DEALLOCATE(v,&
 dprev2,q1prev2,q2prev2,yprev2,s1prev2,s2prev2,hprev2,aprev2,p1prev2,p2prev2,xprev2,rprev2,fprev2,gprev2,Qprev2,betaprev2,&
 dprev,q1prev,q2prev,yprev,s1prev,s2prev,hprev,aprev,p1prev,p2prev,xprev,rprev,fprev,gprev,Qprev,betaprev,&
 dnext,q1next,q2next,ynext,s1next,s2next,hnext,anext,p1next,p2next,xnext,rnext,fnext,gnext,Qnext,betanext,&
 R4v,R4u,czyR4)


30 FORMAT(3(1X,F15.10))
50 FORMAT(5(1X,F15.10))
60 FORMAT(6(1X,F15.10))
70 FORMAT(7(1X,F15.10))
80 FORMAT(8(1X,F15.10))

CLOSE(1)
!CLOSE(2)
!CLOSE(3)
CLOSE(4)
!CLOSE(5)
!CLOSE(6)
!CLOSE(7)
!CLOSE(8)
!CLOSE(9)
!CLOSE(88)

END Program DilLoveCollapse
