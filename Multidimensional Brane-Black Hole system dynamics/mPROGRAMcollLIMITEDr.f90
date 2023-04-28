Program expulsionColl     !WIELOMIAN(nr pktu, nr wielomianu)
                          !WAR BRZEG spelnione automatycznie - rownania pozostaja niepodstawione
USE nrtype
USE LU
USE LUlinsolv
USE module000Parameters
USE module001gestosc

IMPLICIT NONE

INTEGER :: i,j,m,n

REAL(8) :: phi(NCh,NCh),phip(NCh,NCh),phipp(NCh,NCh)   !wspolczynniki phi(m,n) w kolejnych czasach

INTEGER :: t   !licznik czasu
REAL(8) :: x(Nx),y(Ny),r(Ny),theta(Nx)   !pkty siatki w kierunkach x, y, r, theta
REAL(8) :: Tx(Nx,NCh),Ty(Ny,NCh)  !wartosci wielomianow Chebysheva I rodzaju na siatce (x,y)
REAL(8) :: f1(Ny),f2(Ny)   !fkcje z metryki na siatce (x,y)
REAL(8) :: FI(Nx,Ny)   !funkcja PHI w kolejnych czasach na siatce (x,y)

!metoda kolokacyjna
INTEGER, PARAMETER :: Ncoll=NCh,np=Ncoll*Ncoll   !liczba pktow kolokacyjnych w kierunkach x i y oraz rownan w ukladzie = rzad macierzy a
REAL(8) :: Pcoll(Ncoll)   !pkty kolokacyjne
REAL(8) :: rcoll(Ncoll),thetacoll(Ncoll)   !r i theta w pktach kolokacyjnych
REAL(8) :: Ucoll(Ncoll,NCh),Tcoll(Ncoll,NCh)   !wartosci wielomianow Chebysheva w pktach kolokacyjnych
REAL(8) :: f1coll(Ncoll),f2coll(Ncoll),f1Pcoll(Ncoll),f2Pcoll(Ncoll)   !fkcje z metryki w pktach kolokacyjnych oraz ich pochodne
REAL(8) :: FIcoll(Ncoll,Ncoll),FIcollp(Ncoll,Ncoll),FIcollpp(Ncoll,Ncoll),VpoFIcoll(Ncoll,Ncoll)   !obliczona ze wspolczynnikow rozwiniecia fkcja PHI oraz jej pochodna w pktach kolokacyjnych
!- warunki poczatkowe na t=1(0) i t=2(dt)
REAL(8) :: z(Ncoll+1),Tz(Ncoll+1,Ncoll)   !zera wielomianu Chebysheva Ncoll+1 oraz wartosci wielomianow Chebysheva w tych pktach do aproksymacji
REAL(8) :: dFIcoll(Ncoll+1,Ncoll+1)   !wartosci fkcji aproksymowanej w miejscach zerowych wielomianu Chebysheva Ncoll+1
!- rozwiazywanie ukladu rownan liniowych
REAL(8) :: a(np,np),b(np),fun,dxTcoll,dyTcoll,dxxTcollpom,dyyTcoll
INTEGER :: indx(np),xC,yC,mC,nC

!wypisywanie wynikow
REAL(8) :: rho1(Ncoll,Ncoll),rho2(Ncoll,Ncoll)   !gestosc energii


                                 !x=cos(theta),y=f(r),theta,r,R=rsin(theta),Z=rcos(theta),FI,rho
OPEN(16,file='PHIRHOt-5000.dat')
OPEN(25,file='PHIRHOt-7500.dat')
OPEN(35,file='PHIRHOt-10000.dat')


!******* pkty kolokacyjne ******* FULLY CHECKED
DO i=1,Ncoll !Pcoll(Ncoll)
   Pcoll(i)=-DCOS((i-1.D0)*PI/(Ncoll-1.D0))
END DO

!******* (r,theta) w pktach kolokacyjnych ******* +
rcoll=0.5D0*(Pcoll*(rI-rP)+rI+rP) !rcoll(Ncoll)
thetacoll=DACOS(Pcoll) !thetacoll(Ncoll)

!******* fkcje z metryki w pktach kolokacyjnych i ich pochodne ******* +
f1coll=1.D0-(2.D0*rP/(Pcoll*(rI-rP)+rI+rP))**(d-3.D0) !f1coll(Ncoll)
f2coll=1.D0-(2.D0*rM/(Pcoll*(rI-rP)+rI+rP))**(d-3.D0) !f2coll(Ncoll)
f1Pcoll=2.D0*rP*(d-3.D0)*(rI-rP)*(2.D0*rP/(Pcoll*(rI-rP)+rI+rP))**(d-4.D0)/((Pcoll*(rI-rP)+rI+rP)*(Pcoll*(rI-rP)+rI+rP)) !f1Pcoll(NcollY)
f2Pcoll=2.D0*rM*(d-3.D0)*(rI-rP)*(2.D0*rM/(Pcoll*(rI-rP)+rI+rP))**(d-4.D0)/((Pcoll*(rI-rP)+rI+rP)*(Pcoll*(rI-rP)+rI+rP)) !f2Pcoll(NcollY)

!******* wartosci wielomianow Chebysheva II rodzaju w pktach kolokacyjnych ******* FULLY CHECKED
Ucoll(:,1)=1.D0 !Ucoll(Ncoll,NCh)
Ucoll(:,2)=2.D0*Pcoll
DO i=3,NCh
   Ucoll(:,i)=2.D0*Pcoll*Ucoll(:,i-1)-Ucoll(:,i-2)
END DO

!******* wartosci wielomianow Chebysheva I rodzaju w pktach kolokacyjnych ******* FULLY CHECKED
Tcoll(:,1)=1.D0 !Tcoll(Ncoll,NCh)
Tcoll(:,2)=Pcoll
DO i=3,NCh
   Tcoll(:,i)=2.D0*Pcoll*Tcoll(:,i-1)-Tcoll(:,i-2)
END DO


!**************************************************************************************
!******* war pocz: phi(m,n) na t=1 i t=2 - aproksymacja wielomianami Chebysheva *******
!**************************************************************************************

!******* msca zerowe wielomianu Chebysheva Ncoll+1 ******* FULLY CHECKED
DO i=1,Ncoll+1 !z(Ncoll+1)
   z(i)=-DCOS(0.5D0*(2.D0*i-1.D0)*PI/(Ncoll+1.D0))
END DO

!******* wartosci wielomianow Chebysheva I rodzaju w mscach zerowych wielomianu Chebysheva Ncoll+1 *******
Tz(:,1)=1.D0 !Tz(Ncoll+1,Ncoll)
Tz(:,2)=z
DO i=3,Ncoll
   Tz(:,i)=2.D0*z*Tz(:,i-1)-Tz(:,i-2)
END DO

!******* t=1 *******
t=1

!******* dokladna fkcja PHI w mscach zerowych wielomianu Chebysheva Ncoll+1 na t=1 ******* +
DO j=1,Ncoll+1 !dFIcoll(Ncoll+1,Ncoll+1)
   dFIcoll(:,j)=eta*DTANH(DSQRT(0.5D0*lam)*eta*0.5D0*(z(j)*(rI-rP)+rI+rP)*z/DSQRT(1.D0-v*v))
END DO

phipp=0.D0 !phipp(NCh,NCh)
DO i=1,Ncoll+1
   DO j=1,Ncoll+1
      phipp(1,1)=phipp(1,1)+dFIcoll(i,j)
   END DO
END DO
phipp(1,1)=phipp(1,1)/((NCh+1.D0)*(NCh+1.D0))   !UWAGA!NCh=Ncoll
!*
DO n=2,NCh
   DO i=1,Ncoll+1
      DO j=1,Ncoll+1
         phipp(1,n)=phipp(1,n)+dFIcoll(i,j)*Tz(i,n)
      END DO
   END DO
   phipp(1,n)=2.D0*phipp(1,n)/((NCh+1.D0)*(NCh+1.D0))
END DO
!*
DO m=2,NCh
   DO i=1,Ncoll+1
      DO j=1,Ncoll+1
         phipp(m,1)=phipp(m,1)+dFIcoll(i,j)*Tz(j,m)
      END DO
   END DO
   phipp(m,1)=2.D0*phipp(m,1)/((NCh+1.D0)*(NCh+1.D0))
END DO
!*
DO m=2,NCh
   DO n=2,NCh
      DO i=1,Ncoll+1
         DO j=1,Ncoll+1
            phipp(m,n)=phipp(m,n)+dFIcoll(i,j)*Tz(i,n)*Tz(j,m)
         END DO
      END DO
      phipp(m,n)=4.D0*phipp(m,n)/((NCh+1.D0)*(NCh+1.D0))
   END DO
END DO

!******* aproksymowana fkcja PHI w pktach kolokacyjnych na t=1 *******
FIcollpp=0.D0 !FIcollpp(Ncoll,Ncoll)
DO j=1,Ncoll
   DO m=1,NCh
      DO n=1,NCh
         FIcollpp(:,j)=FIcollpp(:,j)+phipp(m,n)*Tcoll(:,n)*Tcoll(j,m)
      END DO
   END DO
END DO

!******* t=2 *******
t=t+1

!******* dokladna fkcja PHI w mscach zerowych wielomianu Chebysheva Ncoll+1 na t=2 *******
DO j=1,Ncoll+1 !dFIcoll(Ncoll+1,Ncoll+1)
   dFIcoll(:,j)=eta*DTANH(DSQRT(0.5D0*lam)*eta*(0.5D0*(z(j)*(rI-rP)+rI+rP)*z-v*dt)/DSQRT(1.D0-v*v))
END DO

phip=0.D0 !phip(NCh,NCh)
DO i=1,Ncoll+1
   DO j=1,Ncoll+1
      phip(1,1)=phip(1,1)+dFIcoll(i,j)
   END DO
END DO
phip(1,1)=phip(1,1)/((NCh+1.D0)*(NCh+1.D0))
!*
DO n=2,NCh
   DO i=1,Ncoll+1
      DO j=1,Ncoll+1
         phip(1,n)=phip(1,n)+dFIcoll(i,j)*Tz(i,n)
      END DO
   END DO
   phip(1,n)=2.D0*phip(1,n)/((NCh+1.D0)*(NCh+1.D0))
END DO
!*
DO m=2,NCh
   DO i=1,Ncoll+1
      DO j=1,Ncoll+1
         phip(m,1)=phip(m,1)+dFIcoll(i,j)*Tz(j,m)
      END DO
   END DO
   phip(m,1)=2.D0*phip(m,1)/((NCh+1.D0)*(NCh+1.D0))
END DO
!*
DO m=2,NCh
   DO n=2,NCh
      DO i=1,Ncoll+1
         DO j=1,Ncoll+1
            phip(m,n)=phip(m,n)+dFIcoll(i,j)*Tz(i,n)*Tz(j,m)
         END DO
      END DO
      phip(m,n)=4.D0*phip(m,n)/((NCh+1.D0)*(NCh+1.D0))
   END DO
END DO

!******* aproksymowana fkcja PHI na siatce (x,y) i w pktach kolokacyjnych na t=2 *******
FIcollp=0.D0 !FIcollp(Ncoll,Ncoll)
DO j=1,Ncoll
   DO m=1,NCh
      DO n=1,NCh
         FIcollp(:,j)=FIcollp(:,j)+phip(m,n)*Tcoll(:,n)*Tcoll(j,m)
      END DO
   END DO
END DO

!******* pochodna V po PHI w pktach kolokacyjnych na t=2 *******
VpoFIcoll=lam*FIcollp*(FIcollp*FIcollp-eta*eta) !VpoFIcoll(Ncoll,Ncoll)


!******************************************
!******* ewolucja phi(m,n) w czasie *******
DO WHILE (t.LT.tk)
!******************************************
t=t+1

!******* elementy macierzy a oraz wyrazy wolne b *******
a=0.D0
b=0.D0
DO i=1,np !nr rnia = nr pktu kolokacyjnego
   xC=ceiling(real(i)/real(Ncoll))   !nr pktu kolokacyjnego w kierunku x
     !CEILING(A) returns the least integer greater than or equal to A
   yC=mod(i,Ncoll)   !nr pktu kolokacyjnego w kierunku y
     !MOD(A,P) computes the remainder of the division of A by P
   IF (yC.EQ.0) yC=Ncoll
   !*
   !* elementy wew i war brzeg: elementy zew x oraz odpowiadajace wyrazy wolne *******
   IF ((yC.NE.1).AND.(yC.NE.Ncoll)) THEN
      DO j=1,np !nr zmiennej phi(m,n)
         mC=ceiling(real(j)/real(Ncoll))   !m z phi(m,n)
         nC=mod(j,Ncoll)   !n z phi(m,n)
         IF (nC.EQ.0) nC=Ncoll
         !
         IF (nC.EQ.1) THEN
            dxTcoll=0.D0
            dxxTcollpom=0.D0
         ELSE
            dxTcoll=nC*Ucoll(xC,nC-1)
            dxxTcollpom=nC*Tcoll(xC,nC)-Pcoll(xC)*Ucoll(xC,nC-1)
         END IF
         IF (mC.EQ.1) THEN
            dyTcoll=0.D0
            dyyTcoll=0.D0
         ELSE
            dyTcoll=mC*Ucoll(yC,mC-1)
            dyyTcoll=mC/(Pcoll(yC)*Pcoll(yC)-1.D0)*(mC*Tcoll(yC,mC)-Pcoll(yC)*Ucoll(yC,mC-1))
         END IF
         !
         a(i,j)=Tcoll(xC,nC)*Tcoll(yC,mC)/(dt*dt)&
          -4.D0*f1coll(yC)*f2coll(yC)**(1.D0-gam*(d-2.D0))/((rI-rP)*(rI-rP)*(Pcoll(yC)*(rI-rP)+rI+rP)**(d-2.D0))&
          *Tcoll(xC,nC)*((d-2.D0)*(rI-rP)*(Pcoll(yC)*(rI-rP)+rI+rP)**(d-3.D0)*f1coll(yC)*f2coll(yC)*dyTcoll&
          +(Pcoll(yC)*(rI-rP)+rI+rP)**(d-2.D0)&
          *((f1Pcoll(yC)*f2coll(yC)+f1coll(yC)*f2Pcoll(yC))*dyTcoll+f1coll(yC)*f2coll(yC)*dyyTcoll))&
          -4.D0*f1coll(yC)*f2coll(yC)**(1.D0-gam*(d-3.D0))/((Pcoll(yC)*(rI-rP)+rI+rP)*(Pcoll(yC)*(rI-rP)+rI+rP))&
          *Tcoll(yC,mC)*((2.D0-d)*Pcoll(xC)*dxTcoll-nC*dxxTcollpom)
         !*
         b(i)=b(i)+(2.D0*phip(mC,nC)-phipp(mC,nC))*Tcoll(xC,nC)*Tcoll(yC,mC)/(dt*dt)
      END DO
      !*
      b(i)=b(i)-VpoFIcoll(xC,yC)*f1coll(yC)*f2coll(yC)**(1.D0-gam*(d-3.D0))
   !*
   !* war brzeg: elementy zew y oraz odpowiadajace wyrazy wolne *******
   ELSE IF (yC.EQ.1) THEN
      DO j=1,np !nr zmiennej phi(m,n)
         mC=ceiling(real(j)/real(Ncoll))
         nC=mod(j,Ncoll)
         IF (nC.EQ.0) nC=Ncoll
         a(i,j)=1.5D0*Tcoll(xC,nC)*Tcoll(yC,mC)/dt
         !*
         b(i)=b(i)+(2.D0*phip(mC,nC)-0.5D0*phipp(mC,nC))*Tcoll(xC,nC)*Tcoll(yC,mC)/dt
      END DO
   !*
   ELSE IF (yC.EQ.Ncoll) THEN
      DO j=1,np !nr zmiennej phi(m,n)
         mC=ceiling(real(j)/real(Ncoll))
         nC=mod(j,Ncoll)
         IF (nC.EQ.0) nC=Ncoll
         a(i,j)=Tcoll(xC,nC)*Tcoll(yC,mC)
      END DO
      !*
      b(i)=b(i)+eta*DTANH(DSQRT(0.5D0*lam)*eta*(rcoll(yC)*Pcoll(xC)-v*t*dt)/DSQRT(1.D0-v*v))
   END IF
END DO

CALL ludcmp(a,indx)
CALL lubksb(a,indx,b)

!******* wspolczynniki phi na t *******
i=1
DO m=1,NCh
   DO n=1,NCh
      phi(m,n)=b(i)
      i=i+1
   END DO
END DO

!******* fkcja PHI na siatce (x,y) i w pktach kolokacyjnych na t *******
FIcoll=0.D0 !FIcoll(Ncoll,Ncoll)
DO j=1,Ncoll
   DO m=1,NCh
      DO n=1,NCh
         FIcoll(:,j)=FIcoll(:,j)+phi(m,n)*Tcoll(:,n)*Tcoll(j,m)
      END DO
   END DO
END DO

!******* pochodna V po PHI w pktach kolokacyjnych na t *******
VpoFIcoll=lam*FIcoll*(FIcoll*FIcoll-eta*eta) !VpoFIcoll(Ncoll,Ncoll)


!* obliczenie gestosci energii w pktach kolokacyjnych na t
!  oraz wypisanie funkcji PHI i gestosci energii do pliku PHIRHOt.dat *
!
IF (t.EQ.5000) THEN
   CALL gestosc(Ncoll,NCh,dt,rP,rI,d,Pcoll,f1coll,f2coll,gam,eta,lam,rcoll,thetacoll,FIcollpp,FIcollp,FIcoll,phi,Tcoll,rho1)!,rho2)
   do i=1,Ncoll
      do j=1,Ncoll
         write(16,90) Pcoll(i),Pcoll(j),thetacoll(i),rcoll(j),rcoll(j)*dsin(thetacoll(i)),rcoll(j)*dcos(thetacoll(i)),&
          FIcoll(i,j),rho1(i,j),rho2(i,j)
      end do
      write(16,*)
   end do
END IF
!
IF (t.EQ.7500) THEN
   CALL gestosc(Ncoll,NCh,dt,rP,rI,d,Pcoll,f1coll,f2coll,gam,eta,lam,rcoll,thetacoll,FIcollpp,FIcollp,FIcoll,phi,Tcoll,rho1)!,rho2)
   do i=1,Ncoll
      do j=1,Ncoll
         write(25,90) Pcoll(i),Pcoll(j),thetacoll(i),rcoll(j),rcoll(j)*dsin(thetacoll(i)),rcoll(j)*dcos(thetacoll(i)),&
          FIcoll(i,j),rho1(i,j),rho2(i,j)
      end do
      write(25,*)
   end do
END IF
!
IF (t.EQ.10000) THEN
   CALL gestosc(Ncoll,NCh,dt,rP,rI,d,Pcoll,f1coll,f2coll,gam,eta,lam,rcoll,thetacoll,FIcollpp,FIcollp,FIcoll,phi,Tcoll,rho1)!,rho2)
   do i=1,Ncoll
      do j=1,Ncoll
         write(35,90) Pcoll(i),Pcoll(j),thetacoll(i),rcoll(j),rcoll(j)*dsin(thetacoll(i)),rcoll(j)*dcos(thetacoll(i)),&
          FIcoll(i,j),rho1(i,j),rho2(i,j)
      end do
      write(35,*)
   end do
END IF

!******* przepisanie wartosci na nizsze poziomy *******
phipp=phip
phip=phi
FIcollpp=FIcollp
FIcollp=FIcoll

!****************************************
!******* koniec ewolucji w czasie *******
END DO
!****************************************

90 FORMAT(9(F20.10))

CLOSE(16)
CLOSE(25)
CLOSE(35)

END Program expulsionColl
