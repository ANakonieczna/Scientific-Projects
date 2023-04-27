MODULE module001gestosc

CONTAINS
SUBROUTINE gestosc(Ncoll,NCh,dt,rP,rI,d,Pcoll,f1coll,f2coll,gam,eta,lam,rcoll,thetacoll,FIcollpp,FIcollp,FIcoll,phi,Tcoll,rho1)!,rho2)
!USE module000pochodne
IMPLICIT NONE
INTEGER,INTENT(IN) :: Ncoll,NCh,d
REAL(8),INTENT(IN) :: dt,rP,rI,gam,eta,lam
REAL(8),DIMENSION(:),INTENT(IN) :: Pcoll,f1coll,f2coll,rcoll,thetacoll
REAL(8),DIMENSION(:,:),INTENT(IN) :: FIcollpp,FIcollp,FIcoll,phi,Tcoll
REAL(8),DIMENSION(:,:),INTENT(OUT) :: rho1!,rho2
INTEGER :: i,j
REAL(8) :: dxF,dyF
!REAL(8),DIMENSION(Ncoll,Ncoll) :: dxFIcoll,dyFIcoll

!******* gestosc energii w pktach kolokacyjnych na t *******
DO i=1,Ncoll
   DO j=1,Ncoll
      !
      IF ((i.NE.1).AND.(i.NE.Ncoll)) dxF=(FIcoll(i+1,j)-FIcoll(i-1,j))/(Pcoll(i+1)-Pcoll(i-1))
      IF ((j.NE.1).AND.(j.NE.Ncoll)) dyF=(FIcoll(i,j+1)-FIcoll(i,j-1))/(Pcoll(j+1)-Pcoll(j-1))
      !
      IF (i.EQ.1) dxF=(FIcoll(i+1,j)-FIcoll(i,j))/(Pcoll(i+1)-Pcoll(i))
      IF (i.EQ.Ncoll) dxF=(FIcoll(i,j)-FIcoll(i-1,j))/(Pcoll(i)-Pcoll(i-1))
      IF (j.EQ.1) dyF=(FIcoll(i,j+1)-FIcoll(i,j))/(Pcoll(j+1)-Pcoll(j))
      IF (j.EQ.Ncoll) dyF=(FIcoll(i,j)-FIcoll(i,j-1))/(Pcoll(j)-Pcoll(j-1))
      !
      rho1(i,j)=(0.5D0*FIcollpp(i,j)-2.D0*FIcollp(i,j)+1.5D0*FIcoll(i,j))&
       *(0.5D0*FIcollpp(i,j)-2.D0*FIcollp(i,j)+1.5D0*FIcoll(i,j))&
       /(dt*dt*f1coll(j)*f2coll(j)**(1.D0-gam*(d-3.D0)))&
       +f1coll(j)*f2coll(j)**(1.D0-gam)/((rI-rP)*(rI-rP))*dyF*dyF&
       +DSIN(thetacoll(i))*DSIN(thetacoll(i))/(rcoll(j)*rcoll(j))*dxF*dxF&
       +0.125D0*lam*(FIcoll(i,j)*FIcoll(i,j)-eta*eta)*(FIcoll(i,j)*FIcoll(i,j)-eta*eta)
   END DO
END DO

!CALL pochodne(Ncoll,NCh,Tcoll,Tcoll,phi,dxFIcoll,dyFIcoll)
!
!DO i=1,Ncoll
!   DO j=1,Ncoll
!      rho2(i,j)=(0.5D0*FIcollpp(i,j)-2.D0*FIcollp(i,j)+1.5D0*FIcoll(i,j))&
!       *(0.5D0*FIcollpp(i,j)-2.D0*FIcollp(i,j)+1.5D0*FIcoll(i,j))&
!       /(dt*dt*f1coll(j)*f2coll(j)**(1.D0-gam*(d-3.D0)))&
!       +f1coll(j)*f2coll(j)**(1.D0-gam)/((rI-rP)*(rI-rP))*dyFIcoll(i,j)*dyFIcoll(i,j)&
!       +DSIN(thetacoll(i))*DSIN(thetacoll(i))/(rcoll(j)*rcoll(j))*dxFIcoll(i,j)*dxFIcoll(i,j)&
!       +0.125D0*lam*(FIcoll(i,j)*FIcoll(i,j)-eta*eta)*(FIcoll(i,j)*FIcoll(i,j)-eta*eta)
!   END DO
!END DO


END SUBROUTINE gestosc

END MODULE module001gestosc
