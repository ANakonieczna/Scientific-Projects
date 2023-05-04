MODULE moduleRK2du

CONTAINS
SUBROUTINE RK2du(h0V,h0U,N,pocz,czyAMR,licznik,&
 d,q1,q2,y,s1,s2,h,a,p1,p2,x,r,f,g,Q,beta,&
 s1prev2,s2prev2,hprev2,aprev2,p1prev2,p2prev2,xprev2,fprev2,Qprev2,betaprev2,&
 s1next,s2next,hnext,anext,p1next,p2next,xnext,fnext,Qnext,betanext,&
 !OUT:
 k2d,k2q1,k2q2,k2y)
!
USE moduleWarunkiBrzegowe
USE moduleABM2ewolucjaV
USE moduleRK2k1234u
USE moduleTworzenieTablicyFUNpppn
USE moduleWartosciFunkcjiAMRW1IF2
USE moduleCSIwsp
IMPLICIT NONE
REAL(8),INTENT(IN) :: h0V,h0U
INTEGER,INTENT(IN) :: N,pocz
LOGICAL,INTENT(IN) :: czyAMR
INTEGER,INTENT(IN) :: licznik
REAL(8),DIMENSION(:),INTENT(IN) :: d,q1,q2,y,s1,s2,h,a,p1,p2,x,r,f,g,Q,beta
REAL(8),INTENT(IN) :: s1prev2,s2prev2,hprev2,aprev2,p1prev2,p2prev2,xprev2,fprev2,Qprev2,betaprev2
REAL(8),INTENT(IN) :: s1next,s2next,hnext,anext,p1next,p2next,xnext,fnext,Qnext,betanext
REAL(8),DIMENSION(:),INTENT(OUT) :: k2d,k2q1,k2q2,k2y
!
REAL(8),DIMENSION(N) :: d_mod,q1_mod,q2_mod,y_mod
REAL(8),DIMENSION(N) :: s1_mod,s2_mod,h_mod,a_mod,p1_mod,p2_mod,x_mod,r_mod,f_mod,g_mod,Q_mod,beta_mod
REAL(8),DIMENSION(N) :: k1d,k1q1,k1q2,k1y
REAL(8) :: s1AMRCSI(3),s2AMRCSI(3),hAMRCSI(3),aAMRCSI(3),p1AMRCSI(3),p2AMRCSI(3),xAMRCSI(3),fAMRCSI(3),QAMRCSI(3),betaAMRCSI(3)
REAL(8) :: aCSI(3),bCSI(3),cCSI(3),dCSI(3)
REAL(8) :: coef,coef2,coef3
INTEGER :: i

!******* RK2-k1 **************************************************************************************************************
DO i=pocz,N
   CALL k1234u(s1(i),s2(i),h(i),q1(i),q2(i),y(i),a(i),p1(i),p2(i),x(i),r(i),f(i),g(i),Q(i),beta(i),&
    !OUT:
    k1d(i),k1q1(i),k1q2(i),k1y(i))
END DO

!* do RK2-k2 *
!******* fkcje mod *******
d_mod=d+0.5D0*h0U*k1d
q1_mod=q1+0.5D0*h0U*k1q1
q2_mod=q2+0.5D0*h0U*k1q2
y_mod=y+0.5D0*h0U*k1y

!******* war brzeg *******
IF (czyAMR) THEN
   IF (licznik.EQ.2) THEN
      CALL WartosciFunkcjiAMRw1IF2(2,2,&
       s1(pocz),s2(pocz),h(pocz),a(pocz),p1(pocz),p2(pocz),x(pocz),f(pocz),Q(pocz),beta(pocz),&
       s1next,s2next,hnext,anext,p1next,p2next,xnext,fnext,Qnext,betanext,&
       !OUT:
       s1_mod(pocz),s2_mod(pocz),h_mod(pocz),a_mod(pocz),p1_mod(pocz),p2_mod(pocz),x_mod(pocz),f_mod(pocz),&
       Q_mod(pocz),beta_mod(pocz))
    ELSE
      !  TWORZENIE TABLIC INTERPOLACYJNYCH CSI !
      CALL TworzenieTablicyFUNpppn(s1prev2,s1(pocz),s1next,s1AMRCSI)
      CALL TworzenieTablicyFUNpppn(s2prev2,s2(pocz),s2next,s2AMRCSI)
      CALL TworzenieTablicyFUNpppn(hprev2,h(pocz),hnext,hAMRCSI)
      CALL TworzenieTablicyFUNpppn(aprev2,a(pocz),anext,aAMRCSI)
      CALL TworzenieTablicyFUNpppn(p1prev2,p1(pocz),p1next,p1AMRCSI)
      CALL TworzenieTablicyFUNpppn(p2prev2,p2(pocz),p2next,p2AMRCSI)
      CALL TworzenieTablicyFUNpppn(xprev2,x(pocz),xnext,xAMRCSI)
      CALL TworzenieTablicyFUNpppn(fprev2,f(pocz),fnext,fAMRCSI)
      CALL TworzenieTablicyFUNpppn(Qprev2,Q(pocz),Qnext,QAMRCSI)
      CALL TworzenieTablicyFUNpppn(betaprev2,beta(pocz),betanext,betaAMRCSI)
      !  INTERPOLACJA CSI !
      coef=0.5D0*h0U
      coef2=coef*coef
      coef3=coef2*coef
      CALL CubicSplineInterpolationWSP(h0U,3,s1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s1_mod(pocz)=aCSI(2)+bCSI(2)*coef+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0U,3,s2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      s2_mod(pocz)=aCSI(2)+bCSI(2)*coef+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0U,3,hAMRCSI,aCSI,bCSI,cCSI,dCSI)
      h_mod(pocz)=aCSI(2)+bCSI(2)*coef+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0U,3,aAMRCSI,aCSI,bCSI,cCSI,dCSI)
      a_mod(pocz)=aCSI(2)+bCSI(2)*coef+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0U,3,p1AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p1_mod(pocz)=aCSI(2)+bCSI(2)*coef+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0U,3,p2AMRCSI,aCSI,bCSI,cCSI,dCSI)
      p2_mod(pocz)=aCSI(2)+bCSI(2)*coef+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0U,3,xAMRCSI,aCSI,bCSI,cCSI,dCSI)
      x_mod(pocz)=aCSI(2)+bCSI(2)*coef+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0U,3,fAMRCSI,aCSI,bCSI,cCSI,dCSI)
      f_mod(pocz)=aCSI(2)+bCSI(2)*coef+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0U,3,QAMRCSI,aCSI,bCSI,cCSI,dCSI)
      Q_mod(pocz)=aCSI(2)+bCSI(2)*coef+cCSI(2)*coef2+dCSI(2)*coef3
      CALL CubicSplineInterpolationWSP(h0U,3,betaAMRCSI,aCSI,bCSI,cCSI,dCSI)
      beta_mod(pocz)=aCSI(2)+bCSI(2)*coef+cCSI(2)*coef2+dCSI(2)*coef3
   END IF
 ELSE
   s1_mod(pocz)=0.D0
   s2_mod(pocz)=0.D0
   h_mod(pocz)=0.D0
   a_mod(pocz)=1.D0
   Q_mod(pocz)=0.D0
   beta_mod(pocz)=0.D0
   f_mod(pocz)=-0.5D0
   p1_mod(pocz)=0.D0
   p2_mod(pocz)=0.D0
   x_mod(pocz)=0.D0
END IF
!
CALL WarunkiBrzegowe(0.5D0*h0U,&
 r(pocz),g(pocz),h_mod(pocz),a_mod(pocz),f_mod(pocz),Q_mod(pocz),&
 !OUT:
 r_mod(pocz),g_mod(pocz))

CALL EwolucjaV(h0V,N,pocz,&
 d_mod,q1_mod,q2_mod,y_mod,&
 !OUT:
 s1_mod,s2_mod,h_mod,a_mod,p1_mod,p2_mod,x_mod,r_mod,f_mod,g_mod,Q_mod,beta_mod)

!******* RK2-k2 **************************************************************************************************************
DO i=pocz,N
   CALL k1234u(s1_mod(i),s2_mod(i),h_mod(i),q1_mod(i),q2_mod(i),y_mod(i),a_mod(i),p1_mod(i),p2_mod(i),x_mod(i),&
    r_mod(i),f_mod(i),g_mod(i),Q_mod(i),beta_mod(i),&
    !OUT:
    k2d(i),k2q1(i),k2q2(i),k2y(i))
END DO

END SUBROUTINE RK2du

END MODULE moduleRK2du
