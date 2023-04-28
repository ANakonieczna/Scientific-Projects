MODULE module111Pochodne

CONTAINS
SUBROUTINE Pochodne(k1,k2,Nh,KK,&
 !OUT:
 dxNh,dyNh,dxxNh,dyyNh,dxyNh,dxKK,dyKK,dxk1,dyk1,dxk2,dyk2)
!
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy
INTEGER :: Nx,Ny
REAL(8) :: p2
COMMON /PARAMS/ M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy,Nx,Ny,p2
!*************************
REAL(8),DIMENSION(:,:),INTENT(IN) :: k1,k2,Nh,KK
REAL(8),DIMENSION(:,:),INTENT(OUT) :: dxNh,dyNh,dxxNh,dyyNh,dxyNh,dxKK,dyKK,dxk1,dyk1,dxk2,dyk2
INTEGER :: i,j

!******* pochodne fkcji k1,k2,Nh,KK na siatce (x,y), bez pktow brzegowych *******
!
!*** center 4th
!* x,y - centered,centered
DO i=3,Nx-2
 DO j=3,Ny-2
    dxk1(i,j)=(k1(i-2,j)-8.D0*k1(i-1,j)+8.D0*k1(i+1,j)-k1(i+2,j))/(12.D0*hx)
    dyk1(i,j)=(k1(i,j-2)-8.D0*k1(i,j-1)+8.D0*k1(i,j+1)-k1(i,j+2))/(12.D0*hy)
    !
    dxk2(i,j)=(k2(i-2,j)-8.D0*k2(i-1,j)+8.D0*k2(i+1,j)-k2(i+2,j))/(12.D0*hx)
    dyk2(i,j)=(k2(i,j-2)-8.D0*k2(i,j-1)+8.D0*k2(i,j+1)-k2(i,j+2))/(12.D0*hy)
    !
    dxNh(i,j)=(Nh(i-2,j)-8.D0*Nh(i-1,j)+8.D0*Nh(i+1,j)-Nh(i+2,j))/(12.D0*hx)
    dyNh(i,j)=(Nh(i,j-2)-8.D0*Nh(i,j-1)+8.D0*Nh(i,j+1)-Nh(i,j+2))/(12.D0*hy)
    dxxNh(i,j)=(-Nh(i-2,j)+16.D0*Nh(i-1,j)-30.D0*Nh(i,j)+16.D0*Nh(i+1,j)-Nh(i+2,j))/(12.D0*hx*hx)
    dyyNh(i,j)=(-Nh(i,j-2)+16.D0*Nh(i,j-1)-30.D0*Nh(i,j)+16.D0*Nh(i,j+1)-Nh(i,j+2))/(12.D0*hy*hy)
    dxyNh(i,j)=(8.D0*(Nh(i+1,j-2)+Nh(i+2,j-1)+Nh(i-2,j+1)+Nh(i-1,j+2)-Nh(i-1,j-2)-Nh(i-2,j-1)-Nh(i+1,j+2)-Nh(i+2,j+1))&
     +Nh(i-2,j-2)+Nh(i+2,j+2)-Nh(i+2,j-2)-Nh(i-2,j+2)+64.D0*(Nh(i-1,j-1)+Nh(i+1,j+1)-Nh(i+1,j-1)-Nh(i-1,j+1)))/(144.D0*hx*hy)
    !
    dxKK(i,j)=(KK(i-2,j)-8.D0*KK(i-1,j)+8.D0*KK(i+1,j)-KK(i+2,j))/(12.D0*hx)
    dyKK(i,j)=(KK(i,j-2)-8.D0*KK(i,j-1)+8.D0*KK(i,j+1)-KK(i,j+2))/(12.D0*hy)
 END DO
END DO

!*** corners 6th
!* x,y - forward,forward
i=2
j=2
dxk1(i,j)=-(-2.D0*k1(i+5,j)+15.D0*k1(i+4,j)-50.D0*k1(i+3,j)+100.D0*k1(i+2,j)-150.D0*k1(i+1,j)+77.D0*k1(i,j)+10.D0*k1(i-1,j))&
          /(60.D0*hx)
dyk1(i,j)=-(-2.D0*k1(i,j+5)+15.D0*k1(i,j+4)-50.D0*k1(i,j+3)+100.D0*k1(i,j+2)-150.D0*k1(i,j+1)+77.D0*k1(i,j)+10.D0*k1(i,j-1))&
          /(60.D0*hy)
!
dxk2(i,j)=-(-2.D0*k2(i+5,j)+15.D0*k2(i+4,j)-50.D0*k2(i+3,j)+100.D0*k2(i+2,j)-150.D0*k2(i+1,j)+77.D0*k2(i,j)+10.D0*k2(i-1,j))&
          /(60.D0*hx)
dyk2(i,j)=-(-2.D0*k2(i,j+5)+15.D0*k2(i,j+4)-50.D0*k2(i,j+3)+100.D0*k2(i,j+2)-150.D0*k2(i,j+1)+77.D0*k2(i,j)+10.D0*k2(i,j-1))&
          /(60.D0*hy)
!
dxNh(i,j)=-(-2.D0*Nh(i+5,j)+15.D0*Nh(i+4,j)-50.D0*Nh(i+3,j)+100.D0*Nh(i+2,j)-150.D0*Nh(i+1,j)+77.D0*Nh(i,j)+10.D0*Nh(i-1,j))&
          /(60.D0*hx)
dyNh(i,j)=-(-2.D0*Nh(i,j+5)+15.D0*Nh(i,j+4)-50.D0*Nh(i,j+3)+100.D0*Nh(i,j+2)-150.D0*Nh(i,j+1)+77.D0*Nh(i,j)+10.D0*Nh(i,j-1))&
          /(60.D0*hy)
dxxNh(i,j)=-(13.D0*Nh(i+5,j)-93.D0*Nh(i+4,j)+285.D0*Nh(i+3,j)-470.D0*Nh(i+2,j)+255.D0*Nh(i+1,j)+147.D0*Nh(i,j)-137.D0*Nh(i-1,j))&
           /(180.D0*hx*hx)
dyyNh(i,j)=-(13.D0*Nh(i,j+5)-93.D0*Nh(i,j+4)+285.D0*Nh(i,j+3)-470.D0*Nh(i,j+2)+255.D0*Nh(i,j+1)+147.D0*Nh(i,j)-137.D0*Nh(i,j-1))&
           /(180.D0*hy*hy)
dxyNh(i,j)=(4.D0*Nh(i+5,j+5)-30.D0*Nh(i+5,j+4)+100.D0*Nh(i+5,j+3)-200.D0*Nh(i+5,j+2)+300.D0*Nh(i+5,j+1)&
            -154.D0*Nh(i+5,j)-20.D0*Nh(i+5,j-1)&
           -7.5D0*(4.D0*Nh(i+4,j+5)-30.D0*Nh(i+4,j+4)+100.D0*Nh(i+4,j+3)-200.D0*Nh(i+4,j+2)+300.D0*Nh(i+4,j+1)&
            -154.D0*Nh(i+4,j)-20.D0*Nh(i+4,j-1))&
           +25.D0*(4.D0*Nh(i+3,j+5)-30.D0*Nh(i+3,j+4)+100.D0*Nh(i+3,j+3)-200.D0*Nh(i+3,j+2)+300.D0*Nh(i+3,j+1)&
            -154.D0*Nh(i+3,j)-20.D0*Nh(i+3,j-1))&
           -50.D0*(4.D0*Nh(i+2,j+5)-30.D0*Nh(i+2,j+4)+100.D0*Nh(i+2,j+3)-200.D0*Nh(i+2,j+2)+300.D0*Nh(i+2,j+1)&
            -154.D0*Nh(i+2,j)-20.D0*Nh(i+2,j-1))&
           +75.D0*(4.D0*Nh(i+1,j+5)-30.D0*Nh(i+1,j+4)+100.D0*Nh(i+1,j+3)-200.D0*Nh(i+1,j+2)+300.D0*Nh(i+1,j+1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j-1))&
           -38.5D0*(4.D0*Nh(i,j+5)-30.D0*Nh(i,j+4)+100.D0*Nh(i,j+3)-200.D0*Nh(i,j+2)+300.D0*Nh(i,j+1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j-1))&
           -5.D0*(4.D0*Nh(i-1,j+5)-30.D0*Nh(i-1,j+4)+100.D0*Nh(i-1,j+3)-200.D0*Nh(i-1,j+2)+300.D0*Nh(i-1,j+1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j-1)))/(3600.D0*hx*hy)
!
dxKK(i,j)=-(-2.D0*KK(i+5,j)+15.D0*KK(i+4,j)-50.D0*KK(i+3,j)+100.D0*KK(i+2,j)-150.D0*KK(i+1,j)+77.D0*KK(i,j)+10.D0*KK(i-1,j))&
          /(60.D0*hx)
dyKK(i,j)=-(-2.D0*KK(i,j+5)+15.D0*KK(i,j+4)-50.D0*KK(i,j+3)+100.D0*KK(i,j+2)-150.D0*KK(i,j+1)+77.D0*KK(i,j)+10.D0*KK(i,j-1))&
          /(60.D0*hy)
!
!* x,y - back,forward
i=Nx-1
j=2
dxk1(i,j)=(-2.D0*k1(i-5,j)+15.D0*k1(i-4,j)-50.D0*k1(i-3,j)+100.D0*k1(i-2,j)-150.D0*k1(i-1,j)+77.D0*k1(i,j)+10.D0*k1(i+1,j))&
          /(60.D0*hx)
dyk1(i,j)=-(-2.D0*k1(i,j+5)+15.D0*k1(i,j+4)-50.D0*k1(i,j+3)+100.D0*k1(i,j+2)-150.D0*k1(i,j+1)+77.D0*k1(i,j)+10.D0*k1(i,j-1))&
          /(60.D0*hy)
!
dxk2(i,j)=(-2.D0*k2(i-5,j)+15.D0*k2(i-4,j)-50.D0*k2(i-3,j)+100.D0*k2(i-2,j)-150.D0*k2(i-1,j)+77.D0*k2(i,j)+10.D0*k2(i+1,j))&
          /(60.D0*hx)
dyk2(i,j)=-(-2.D0*k2(i,j+5)+15.D0*k2(i,j+4)-50.D0*k2(i,j+3)+100.D0*k2(i,j+2)-150.D0*k2(i,j+1)+77.D0*k2(i,j)+10.D0*k2(i,j-1))&
          /(60.D0*hy)
!
dxNh(i,j)=(-2.D0*Nh(i-5,j)+15.D0*Nh(i-4,j)-50.D0*Nh(i-3,j)+100.D0*Nh(i-2,j)-150.D0*Nh(i-1,j)+77.D0*Nh(i,j)+10.D0*Nh(i+1,j))&
          /(60.D0*hx)
dyNh(i,j)=-(-2.D0*Nh(i,j+5)+15.D0*Nh(i,j+4)-50.D0*Nh(i,j+3)+100.D0*Nh(i,j+2)-150.D0*Nh(i,j+1)+77.D0*Nh(i,j)+10.D0*Nh(i,j-1))&
          /(60.D0*hy)
dxxNh(i,j)=-(13.D0*Nh(i-5,j)-93.D0*Nh(i-4,j)+285.D0*Nh(i-3,j)-470.D0*Nh(i-2,j)+255.D0*Nh(i-1,j)+147.D0*Nh(i,j)-137.D0*Nh(i+1,j))&
          /(180.D0*hx*hx)
dyyNh(i,j)=-(13.D0*Nh(i,j+5)-93.D0*Nh(i,j+4)+285.D0*Nh(i,j+3)-470.D0*Nh(i,j+2)+255.D0*Nh(i,j+1)+147.D0*Nh(i,j)-137.D0*Nh(i,j-1))&
          /(180.D0*hy*hy)
dxyNh(i,j)=-(4.D0*Nh(i-5,j+5)-30.D0*Nh(i-5,j+4)+100.D0*Nh(i-5,j+3)-200.D0*Nh(i-5,j+2)+300.D0*Nh(i-5,j+1)&
            -154.D0*Nh(i-5,j)-20.D0*Nh(i-5,j-1)&
           -7.5D0*(4.D0*Nh(i-4,j+5)-30.D0*Nh(i-4,j+4)+100.D0*Nh(i-4,j+3)-200.D0*Nh(i-4,j+2)+300.D0*Nh(i-4,j+1)&
            -154.D0*Nh(i-4,j)-20.D0*Nh(i-4,j-1))&
           +25.D0*(4.D0*Nh(i-3,j+5)-30.D0*Nh(i-3,j+4)+100.D0*Nh(i-3,j+3)-200.D0*Nh(i-3,j+2)+300.D0*Nh(i-3,j+1)&
            -154.D0*Nh(i-3,j)-20.D0*Nh(i-3,j-1))&
           -50.D0*(4.D0*Nh(i-2,j+5)-30.D0*Nh(i-2,j+4)+100.D0*Nh(i-2,j+3)-200.D0*Nh(i-2,j+2)+300.D0*Nh(i-2,j+1)&
            -154.D0*Nh(i-2,j)-20.D0*Nh(i-2,j-1))&
           +75.D0*(4.D0*Nh(i-1,j+5)-30.D0*Nh(i-1,j+4)+100.D0*Nh(i-1,j+3)-200.D0*Nh(i-1,j+2)+300.D0*Nh(i-1,j+1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j-1))&
           -38.5D0*(4.D0*Nh(i,j+5)-30.D0*Nh(i,j+4)+100.D0*Nh(i,j+3)-200.D0*Nh(i,j+2)+300.D0*Nh(i,j+1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j-1))&
           -5.D0*(4.D0*Nh(i+1,j+5)-30.D0*Nh(i+1,j+4)+100.D0*Nh(i+1,j+3)-200.D0*Nh(i+1,j+2)+300.D0*Nh(i+1,j+1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j-1)))/(3600.D0*hx*hy)
!
dxKK(i,j)=(-2.D0*KK(i-5,j)+15.D0*KK(i-4,j)-50.D0*KK(i-3,j)+100.D0*KK(i-2,j)-150.D0*KK(i-1,j)+77.D0*KK(i,j)+10.D0*KK(i+1,j))&
          /(60.D0*hx)
dyKK(i,j)=-(-2.D0*KK(i,j+5)+15.D0*KK(i,j+4)-50.D0*KK(i,j+3)+100.D0*KK(i,j+2)-150.D0*KK(i,j+1)+77.D0*KK(i,j)+10.D0*KK(i,j-1))&
          /(60.D0*hy)
!
!* x,y - forward,back
i=2
j=Ny-1
dxk1(i,j)=-(-2.D0*k1(i+5,j)+15.D0*k1(i+4,j)-50.D0*k1(i+3,j)+100.D0*k1(i+2,j)-150.D0*k1(i+1,j)+77.D0*k1(i,j)+10.D0*k1(i-1,j))&
          /(60.D0*hx)
dyk1(i,j)=(-2.D0*k1(i,j-5)+15.D0*k1(i,j-4)-50.D0*k1(i,j-3)+100.D0*k1(i,j-2)-150.D0*k1(i,j-1)+77.D0*k1(i,j)+10.D0*k1(i,j+1))&
          /(60.D0*hy)
!
dxk2(i,j)=-(-2.D0*k2(i+5,j)+15.D0*k2(i+4,j)-50.D0*k2(i+3,j)+100.D0*k2(i+2,j)-150.D0*k2(i+1,j)+77.D0*k2(i,j)+10.D0*k2(i-1,j))&
          /(60.D0*hx)
dyk2(i,j)=(-2.D0*k2(i,j-5)+15.D0*k2(i,j-4)-50.D0*k2(i,j-3)+100.D0*k2(i,j-2)-150.D0*k2(i,j-1)+77.D0*k2(i,j)+10.D0*k2(i,j+1))&
          /(60.D0*hy)
!
dxNh(i,j)=-(-2.D0*Nh(i+5,j)+15.D0*Nh(i+4,j)-50.D0*Nh(i+3,j)+100.D0*Nh(i+2,j)-150.D0*Nh(i+1,j)+77.D0*Nh(i,j)+10.D0*Nh(i-1,j))&
          /(60.D0*hx)
dyNh(i,j)=(-2.D0*Nh(i,j-5)+15.D0*Nh(i,j-4)-50.D0*Nh(i,j-3)+100.D0*Nh(i,j-2)-150.D0*Nh(i,j-1)+77.D0*Nh(i,j)+10.D0*Nh(i,j+1))&
          /(60.D0*hy)
dxxNh(i,j)=-(13.D0*Nh(i+5,j)-93.D0*Nh(i+4,j)+285.D0*Nh(i+3,j)-470.D0*Nh(i+2,j)+255.D0*Nh(i+1,j)+147.D0*Nh(i,j)-137.D0*Nh(i-1,j))&
           /(180.D0*hx*hx)
dyyNh(i,j)=-(13.D0*Nh(i,j-5)-93.D0*Nh(i,j-4)+285.D0*Nh(i,j-3)-470.D0*Nh(i,j-2)+255.D0*Nh(i,j-1)+147.D0*Nh(i,j)-137.D0*Nh(i,j+1))&
           /(180.D0*hy*hy)
dxyNh(i,j)=-(4.D0*Nh(i+5,j-5)-30.D0*Nh(i+5,j-4)+100.D0*Nh(i+5,j-3)-200.D0*Nh(i+5,j-2)+300.D0*Nh(i+5,j-1)&
            -154.D0*Nh(i+5,j)-20.D0*Nh(i+5,j+1)&
           -7.5D0*(4.D0*Nh(i+4,j-5)-30.D0*Nh(i+4,j-4)+100.D0*Nh(i+4,j-3)-200.D0*Nh(i+4,j-2)+300.D0*Nh(i+4,j-1)&
            -154.D0*Nh(i+4,j)-20.D0*Nh(i+4,j+1))&
           +25.D0*(4.D0*Nh(i+3,j-5)-30.D0*Nh(i+3,j-4)+100.D0*Nh(i+3,j-3)-200.D0*Nh(i+3,j-2)+300.D0*Nh(i+3,j-1)&
            -154.D0*Nh(i+3,j)-20.D0*Nh(i+3,j+1))&
           -50.D0*(4.D0*Nh(i+2,j-5)-30.D0*Nh(i+2,j-4)+100.D0*Nh(i+2,j-3)-200.D0*Nh(i+2,j-2)+300.D0*Nh(i+2,j-1)&
            -154.D0*Nh(i+2,j)-20.D0*Nh(i+2,j+1))&
           +75.D0*(4.D0*Nh(i+1,j-5)-30.D0*Nh(i+1,j-4)+100.D0*Nh(i+1,j-3)-200.D0*Nh(i+1,j-2)+300.D0*Nh(i+1,j-1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j+1))&
           -38.5D0*(4.D0*Nh(i,j-5)-30.D0*Nh(i,j-4)+100.D0*Nh(i,j-3)-200.D0*Nh(i,j-2)+300.D0*Nh(i,j-1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j+1))&
           -5.D0*(4.D0*Nh(i-1,j-5)-30.D0*Nh(i-1,j-4)+100.D0*Nh(i-1,j-3)-200.D0*Nh(i-1,j-2)+300.D0*Nh(i-1,j-1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j+1)))/(3600.D0*hx*hy)
!
dxKK(i,j)=-(-2.D0*KK(i+5,j)+15.D0*KK(i+4,j)-50.D0*KK(i+3,j)+100.D0*KK(i+2,j)-150.D0*KK(i+1,j)+77.D0*KK(i,j)+10.D0*KK(i-1,j))&
          /(60.D0*hx)
dyKK(i,j)=(-2.D0*KK(i,j-5)+15.D0*KK(i,j-4)-50.D0*KK(i,j-3)+100.D0*KK(i,j-2)-150.D0*KK(i,j-1)+77.D0*KK(i,j)+10.D0*KK(i,j+1))&
          /(60.D0*hy)
!
!* x,y - back,back
i=Nx-1
j=Ny-1
dxk1(i,j)=(-2.D0*k1(i-5,j)+15.D0*k1(i-4,j)-50.D0*k1(i-3,j)+100.D0*k1(i-2,j)-150.D0*k1(i-1,j)+77.D0*k1(i,j)+10.D0*k1(i+1,j))&
          /(60.D0*hx)
dyk1(i,j)=(-2.D0*k1(i,j-5)+15.D0*k1(i,j-4)-50.D0*k1(i,j-3)+100.D0*k1(i,j-2)-150.D0*k1(i,j-1)+77.D0*k1(i,j)+10.D0*k1(i,j+1))&
          /(60.D0*hy)
!
dxk2(i,j)=(-2.D0*k2(i-5,j)+15.D0*k2(i-4,j)-50.D0*k2(i-3,j)+100.D0*k2(i-2,j)-150.D0*k2(i-1,j)+77.D0*k2(i,j)+10.D0*k2(i+1,j))&
          /(60.D0*hx)
dyk2(i,j)=(-2.D0*k2(i,j-5)+15.D0*k2(i,j-4)-50.D0*k2(i,j-3)+100.D0*k2(i,j-2)-150.D0*k2(i,j-1)+77.D0*k2(i,j)+10.D0*k2(i,j+1))&
          /(60.D0*hy)
!
dxNh(i,j)=(-2.D0*Nh(i-5,j)+15.D0*Nh(i-4,j)-50.D0*Nh(i-3,j)+100.D0*Nh(i-2,j)-150.D0*Nh(i-1,j)+77.D0*Nh(i,j)+10.D0*Nh(i+1,j))&
          /(60.D0*hx)
dyNh(i,j)=(-2.D0*Nh(i,j-5)+15.D0*Nh(i,j-4)-50.D0*Nh(i,j-3)+100.D0*Nh(i,j-2)-150.D0*Nh(i,j-1)+77.D0*Nh(i,j)+10.D0*Nh(i,j+1))&
          /(60.D0*hy)
dxxNh(i,j)=-(13.D0*Nh(i-5,j)-93.D0*Nh(i-4,j)+285.D0*Nh(i-3,j)-470.D0*Nh(i-2,j)+255.D0*Nh(i-1,j)+147.D0*Nh(i,j)-137.D0*Nh(i+1,j))&
           /(180.D0*hx*hx)
dyyNh(i,j)=-(13.D0*Nh(i,j-5)-93.D0*Nh(i,j-4)+285.D0*Nh(i,j-3)-470.D0*Nh(i,j-2)+255.D0*Nh(i,j-1)+147.D0*Nh(i,j)-137.D0*Nh(i,j+1))&
           /(180.D0*hy*hy)
dxyNh(i,j)=(4.D0*Nh(i-5,j-5)-30.D0*Nh(i-5,j-4)+100.D0*Nh(i-5,j-3)-200.D0*Nh(i-5,j-2)+300.D0*Nh(i-5,j-1)&
            -154.D0*Nh(i-5,j)-20.D0*Nh(i-5,j+1)&
           -7.5D0*(4.D0*Nh(i-4,j-5)-30.D0*Nh(i-4,j-4)+100.D0*Nh(i-4,j-3)-200.D0*Nh(i-4,j-2)+300.D0*Nh(i-4,j-1)&
            -154.D0*Nh(i-4,j)-20.D0*Nh(i-4,j+1))&
           +25.D0*(4.D0*Nh(i-3,j-5)-30.D0*Nh(i-3,j-4)+100.D0*Nh(i-3,j-3)-200.D0*Nh(i-3,j-2)+300.D0*Nh(i-3,j-1)&
            -154.D0*Nh(i-3,j)-20.D0*Nh(i-3,j+1))&
           -50.D0*(4.D0*Nh(i-2,j-5)-30.D0*Nh(i-2,j-4)+100.D0*Nh(i-2,j-3)-200.D0*Nh(i-2,j-2)+300.D0*Nh(i-2,j-1)&
            -154.D0*Nh(i-2,j)-20.D0*Nh(i-2,j+1))&
           +75.D0*(4.D0*Nh(i-1,j-5)-30.D0*Nh(i-1,j-4)+100.D0*Nh(i-1,j-3)-200.D0*Nh(i-1,j-2)+300.D0*Nh(i-1,j-1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j+1))&
           -38.5D0*(4.D0*Nh(i,j-5)-30.D0*Nh(i,j-4)+100.D0*Nh(i,j-3)-200.D0*Nh(i,j-2)+300.D0*Nh(i,j-1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j+1))&
           -5.D0*(4.D0*Nh(i+1,j-5)-30.D0*Nh(i+1,j-4)+100.D0*Nh(i+1,j-3)-200.D0*Nh(i+1,j-2)+300.D0*Nh(i+1,j-1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j+1)))/(3600.D0*hx*hy)
!
dxKK(i,j)=(-2.D0*KK(i-5,j)+15.D0*KK(i-4,j)-50.D0*KK(i-3,j)+100.D0*KK(i-2,j)-150.D0*KK(i-1,j)+77.D0*KK(i,j)+10.D0*KK(i+1,j))&
          /(60.D0*hx)
dyKK(i,j)=(-2.D0*KK(i,j-5)+15.D0*KK(i,j-4)-50.D0*KK(i,j-3)+100.D0*KK(i,j-2)-150.D0*KK(i,j-1)+77.D0*KK(i,j)+10.D0*KK(i,j+1))&
          /(60.D0*hy)

!*** borders 6th
!* x,y - forward,centered
i=2
DO j=3,Ny-2
   dxk1(i,j)=-(-2.D0*k1(i+5,j)+15.D0*k1(i+4,j)-50.D0*k1(i+3,j)+100.D0*k1(i+2,j)-150.D0*k1(i+1,j)+77.D0*k1(i,j)+10.D0*k1(i-1,j))&
             /(60.D0*hx)
   dyk1(i,j)=(k1(i,j-2)-8.D0*k1(i,j-1)+8.D0*k1(i,j+1)-k1(i,j+2))/(12.D0*hy)
   !
   dxk2(i,j)=-(-2.D0*k2(i+5,j)+15.D0*k2(i+4,j)-50.D0*k2(i+3,j)+100.D0*k2(i+2,j)-150.D0*k2(i+1,j)+77.D0*k2(i,j)+10.D0*k2(i-1,j))&
             /(60.D0*hx)
   dyk2(i,j)=(k2(i,j-2)-8.D0*k2(i,j-1)+8.D0*k2(i,j+1)-k2(i,j+2))/(12.D0*hy)
   !
   dxNh(i,j)=-(-2.D0*Nh(i+5,j)+15.D0*Nh(i+4,j)-50.D0*Nh(i+3,j)+100.D0*Nh(i+2,j)-150.D0*Nh(i+1,j)+77.D0*Nh(i,j)+10.D0*Nh(i-1,j))&
             /(60.D0*hx)
   dyNh(i,j)=(Nh(i,j-2)-8.D0*Nh(i,j-1)+8.D0*Nh(i,j+1)-Nh(i,j+2))/(12.D0*hy)
   dxxNh(i,j)=-(13.D0*Nh(i+5,j)-93.D0*Nh(i+4,j)+285.D0*Nh(i+3,j)-470.D0*Nh(i+2,j)+255.D0*Nh(i+1,j)+147.D0*Nh(i,j)-137.D0*Nh(i-1,j))&
              /(180.D0*hx*hx)
   dyyNh(i,j)=(-Nh(i,j-2)+16.D0*Nh(i,j-1)-30.D0*Nh(i,j)+16.D0*Nh(i,j+1)-Nh(i,j+2))/(12.D0*hy*hy)
   !
   dxKK(i,j)=-(-2.D0*KK(i+5,j)+15.D0*KK(i+4,j)-50.D0*KK(i+3,j)+100.D0*KK(i+2,j)-150.D0*KK(i+1,j)+77.D0*KK(i,j)+10.D0*KK(i-1,j))&
             /(60.D0*hx)
   dyKK(i,j)=(KK(i,j-2)-8.D0*KK(i,j-1)+8.D0*KK(i,j+1)-KK(i,j+2))/(12.D0*hy)
END DO
!
DO j=4,Ny-3
   dxyNh(i,j)=-(2.D0*Nh(i+5,j-3)-18.D0*Nh(i+5,j-2)+90.D0*Nh(i+5,j-1)-90.D0*Nh(i+5,j+1)+18.D0*Nh(i+5,j+2)-2.D0*Nh(i+5,j+3)&
              -7.5D0*(2.D0*Nh(i+4,j-3)-18.D0*Nh(i+4,j-2)+90.D0*Nh(i+4,j-1)-90.D0*Nh(i+4,j+1)+18.D0*Nh(i+4,j+2)-2.D0*Nh(i+4,j+3))&
              +25.D0*(2.D0*Nh(i+3,j-3)-18.D0*Nh(i+3,j-2)+90.D0*Nh(i+3,j-1)-90.D0*Nh(i+3,j+1)+18.D0*Nh(i+3,j+2)-2.D0*Nh(i+3,j+3))&
              -50.D0*(2.D0*Nh(i+2,j-3)-18.D0*Nh(i+2,j-2)+90.D0*Nh(i+2,j-1)-90.D0*Nh(i+2,j+1)+18.D0*Nh(i+2,j+2)-2.D0*Nh(i+2,j+3))&
              +75.D0*(2.D0*Nh(i+1,j-3)-18.D0*Nh(i+1,j-2)+90.D0*Nh(i+1,j-1)-90.D0*Nh(i+1,j+1)+18.D0*Nh(i+1,j+2)-2.D0*Nh(i+1,j+3))&
              -38.5D0*(2.D0*Nh(i,j-3)-18.D0*Nh(i,j-2)+90.D0*Nh(i,j-1)-90.D0*Nh(i,j+1)+18.D0*Nh(i,j+2)-2.D0*Nh(i,j+3))&
              -5.D0*(2.D0*Nh(i-1,j-3)-18.D0*Nh(i-1,j-2)+90.D0*Nh(i-1,j-1)-90.D0*Nh(i-1,j+1)+18.D0*Nh(i-1,j+2)-2.D0*Nh(i-1,j+3)))&
              /(3600.D0*hx*hy)
END DO
!
j=3
dxyNh(i,j)=(4.D0*Nh(i+5,j+5)-30.D0*Nh(i+5,j+4)+100.D0*Nh(i+5,j+3)-200.D0*Nh(i+5,j+2)+300.D0*Nh(i+5,j+1)&
            -154.D0*Nh(i+5,j)-20.D0*Nh(i+5,j-1)&
           -7.5D0*(4.D0*Nh(i+4,j+5)-30.D0*Nh(i+4,j+4)+100.D0*Nh(i+4,j+3)-200.D0*Nh(i+4,j+2)+300.D0*Nh(i+4,j+1)&
            -154.D0*Nh(i+4,j)-20.D0*Nh(i+4,j-1))&
           +25.D0*(4.D0*Nh(i+3,j+5)-30.D0*Nh(i+3,j+4)+100.D0*Nh(i+3,j+3)-200.D0*Nh(i+3,j+2)+300.D0*Nh(i+3,j+1)&
            -154.D0*Nh(i+3,j)-20.D0*Nh(i+3,j-1))&
           -50.D0*(4.D0*Nh(i+2,j+5)-30.D0*Nh(i+2,j+4)+100.D0*Nh(i+2,j+3)-200.D0*Nh(i+2,j+2)+300.D0*Nh(i+2,j+1)&
            -154.D0*Nh(i+2,j)-20.D0*Nh(i+2,j-1))&
           +75.D0*(4.D0*Nh(i+1,j+5)-30.D0*Nh(i+1,j+4)+100.D0*Nh(i+1,j+3)-200.D0*Nh(i+1,j+2)+300.D0*Nh(i+1,j+1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j-1))&
           -38.5D0*(4.D0*Nh(i,j+5)-30.D0*Nh(i,j+4)+100.D0*Nh(i,j+3)-200.D0*Nh(i,j+2)+300.D0*Nh(i,j+1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j-1))&
           -5.D0*(4.D0*Nh(i-1,j+5)-30.D0*Nh(i-1,j+4)+100.D0*Nh(i-1,j+3)-200.D0*Nh(i-1,j+2)+300.D0*Nh(i-1,j+1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j-1)))/(3600.D0*hx*hy)
!
j=Ny-2
dxyNh(i,j)=-(4.D0*Nh(i+5,j-5)-30.D0*Nh(i+5,j-4)+100.D0*Nh(i+5,j-3)-200.D0*Nh(i+5,j-2)+300.D0*Nh(i+5,j-1)&
            -154.D0*Nh(i+5,j)-20.D0*Nh(i+5,j+1)&
           -7.5D0*(4.D0*Nh(i+4,j-5)-30.D0*Nh(i+4,j-4)+100.D0*Nh(i+4,j-3)-200.D0*Nh(i+4,j-2)+300.D0*Nh(i+4,j-1)&
            -154.D0*Nh(i+4,j)-20.D0*Nh(i+4,j+1))&
           +25.D0*(4.D0*Nh(i+3,j-5)-30.D0*Nh(i+3,j-4)+100.D0*Nh(i+3,j-3)-200.D0*Nh(i+3,j-2)+300.D0*Nh(i+3,j-1)&
            -154.D0*Nh(i+3,j)-20.D0*Nh(i+3,j+1))&
           -50.D0*(4.D0*Nh(i+2,j-5)-30.D0*Nh(i+2,j-4)+100.D0*Nh(i+2,j-3)-200.D0*Nh(i+2,j-2)+300.D0*Nh(i+2,j-1)&
            -154.D0*Nh(i+2,j)-20.D0*Nh(i+2,j+1))&
           +75.D0*(4.D0*Nh(i+1,j-5)-30.D0*Nh(i+1,j-4)+100.D0*Nh(i+1,j-3)-200.D0*Nh(i+1,j-2)+300.D0*Nh(i+1,j-1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j+1))&
           -38.5D0*(4.D0*Nh(i,j-5)-30.D0*Nh(i,j-4)+100.D0*Nh(i,j-3)-200.D0*Nh(i,j-2)+300.D0*Nh(i,j-1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j+1))&
           -5.D0*(4.D0*Nh(i-1,j-5)-30.D0*Nh(i-1,j-4)+100.D0*Nh(i-1,j-3)-200.D0*Nh(i-1,j-2)+300.D0*Nh(i-1,j-1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j+1)))/(3600.D0*hx*hy)
!
!* x,y - back,centered
i=Nx-1
DO j=3,Ny-2
   dxk1(i,j)=(-2.D0*k1(i-5,j)+15.D0*k1(i-4,j)-50.D0*k1(i-3,j)+100.D0*k1(i-2,j)-150.D0*k1(i-1,j)+77.D0*k1(i,j)+10.D0*k1(i+1,j))&
             /(60.D0*hx)
   dyk1(i,j)=(k1(i,j-2)-8.D0*k1(i,j-1)+8.D0*k1(i,j+1)-k1(i,j+2))/(12.D0*hy)
   !
   dxk2(i,j)=(-2.D0*k2(i-5,j)+15.D0*k2(i-4,j)-50.D0*k2(i-3,j)+100.D0*k2(i-2,j)-150.D0*k2(i-1,j)+77.D0*k2(i,j)+10.D0*k2(i+1,j))&
             /(60.D0*hx)
   dyk2(i,j)=(k2(i,j-2)-8.D0*k2(i,j-1)+8.D0*k2(i,j+1)-k2(i,j+2))/(12.D0*hy)
   !
   dxNh(i,j)=(-2.D0*Nh(i-5,j)+15.D0*Nh(i-4,j)-50.D0*Nh(i-3,j)+100.D0*Nh(i-2,j)-150.D0*Nh(i-1,j)+77.D0*Nh(i,j)+10.D0*Nh(i+1,j))&
             /(60.D0*hx)
   dyNh(i,j)=(Nh(i,j-2)-8.D0*Nh(i,j-1)+8.D0*Nh(i,j+1)-Nh(i,j+2))/(12.D0*hy)
   dxxNh(i,j)=-(13.D0*Nh(i-5,j)-93.D0*Nh(i-4,j)+285.D0*Nh(i-3,j)-470.D0*Nh(i-2,j)+255.D0*Nh(i-1,j)+147.D0*Nh(i,j)-137.D0*Nh(i+1,j))&
              /(180.D0*hx*hx)
   dyyNh(i,j)=(-Nh(i,j-2)+16.D0*Nh(i,j-1)-30.D0*Nh(i,j)+16.D0*Nh(i,j+1)-Nh(i,j+2))/(12.D0*hy*hy)
   !
   dxKK(i,j)=(-2.D0*KK(i-5,j)+15.D0*KK(i-4,j)-50.D0*KK(i-3,j)+100.D0*KK(i-2,j)-150.D0*KK(i-1,j)+77.D0*KK(i,j)+10.D0*KK(i+1,j))&
             /(60.D0*hx)
   dyKK(i,j)=(KK(i,j-2)-8.D0*KK(i,j-1)+8.D0*KK(i,j+1)-KK(i,j+2))/(12.D0*hy)
END DO
!
DO j=4,Ny-3
   dxyNh(i,j)=(2.D0*Nh(i-5,j-3)-18.D0*Nh(i-5,j-2)+90.D0*Nh(i-5,j-1)-90.D0*Nh(i-5,j+1)+18.D0*Nh(i-5,j+2)-2.D0*Nh(i-5,j+3)&
              -7.5D0*(2.D0*Nh(i-4,j-3)-18.D0*Nh(i-4,j-2)+90.D0*Nh(i-4,j-1)-90.D0*Nh(i-4,j+1)+18.D0*Nh(i-4,j+2)-2.D0*Nh(i-4,j+3))&
              +25.D0*(2.D0*Nh(i-3,j-3)-18.D0*Nh(i-3,j-2)+90.D0*Nh(i-3,j-1)-90.D0*Nh(i-3,j+1)+18.D0*Nh(i-3,j+2)-2.D0*Nh(i-3,j+3))&
              -50.D0*(2.D0*Nh(i-2,j-3)-18.D0*Nh(i-2,j-2)+90.D0*Nh(i-2,j-1)-90.D0*Nh(i-2,j+1)+18.D0*Nh(i-2,j+2)-2.D0*Nh(i-2,j+3))&
              +75.D0*(2.D0*Nh(i-1,j-3)-18.D0*Nh(i-1,j-2)+90.D0*Nh(i-1,j-1)-90.D0*Nh(i-1,j+1)+18.D0*Nh(i-1,j+2)-2.D0*Nh(i-1,j+3))&
              -38.5D0*(2.D0*Nh(i,j-3)-18.D0*Nh(i,j-2)+90.D0*Nh(i,j-1)-90.D0*Nh(i,j+1)+18.D0*Nh(i,j+2)-2.D0*Nh(i,j+3))&
              -5.D0*(2.D0*Nh(i+1,j-3)-18.D0*Nh(i+1,j-2)+90.D0*Nh(i+1,j-1)-90.D0*Nh(i+1,j+1)+18.D0*Nh(i+1,j+2)-2.D0*Nh(i+1,j+3)))&
              /(3600.D0*hx*hy)
END DO
!
j=3
dxyNh(i,j)=-(4.D0*Nh(i-5,j+5)-30.D0*Nh(i-5,j+4)+100.D0*Nh(i-5,j+3)-200.D0*Nh(i-5,j+2)+300.D0*Nh(i-5,j+1)&
            -154.D0*Nh(i-5,j)-20.D0*Nh(i-5,j-1)&
           -7.5D0*(4.D0*Nh(i-4,j+5)-30.D0*Nh(i-4,j+4)+100.D0*Nh(i-4,j+3)-200.D0*Nh(i-4,j+2)+300.D0*Nh(i-4,j+1)&
            -154.D0*Nh(i-4,j)-20.D0*Nh(i-4,j-1))&
           +25.D0*(4.D0*Nh(i-3,j+5)-30.D0*Nh(i-3,j+4)+100.D0*Nh(i-3,j+3)-200.D0*Nh(i-3,j+2)+300.D0*Nh(i-3,j+1)&
            -154.D0*Nh(i-3,j)-20.D0*Nh(i-3,j-1))&
           -50.D0*(4.D0*Nh(i-2,j+5)-30.D0*Nh(i-2,j+4)+100.D0*Nh(i-2,j+3)-200.D0*Nh(i-2,j+2)+300.D0*Nh(i-2,j+1)&
            -154.D0*Nh(i-2,j)-20.D0*Nh(i-2,j-1))&
           +75.D0*(4.D0*Nh(i-1,j+5)-30.D0*Nh(i-1,j+4)+100.D0*Nh(i-1,j+3)-200.D0*Nh(i-1,j+2)+300.D0*Nh(i-1,j+1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j-1))&
           -38.5D0*(4.D0*Nh(i,j+5)-30.D0*Nh(i,j+4)+100.D0*Nh(i,j+3)-200.D0*Nh(i,j+2)+300.D0*Nh(i,j+1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j-1))&
           -5.D0*(4.D0*Nh(i+1,j+5)-30.D0*Nh(i+1,j+4)+100.D0*Nh(i+1,j+3)-200.D0*Nh(i+1,j+2)+300.D0*Nh(i+1,j+1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j-1)))/(3600.D0*hx*hy)
!
j=Ny-2
dxyNh(i,j)=(4.D0*Nh(i-5,j-5)-30.D0*Nh(i-5,j-4)+100.D0*Nh(i-5,j-3)-200.D0*Nh(i-5,j-2)+300.D0*Nh(i-5,j-1)&
            -154.D0*Nh(i-5,j)-20.D0*Nh(i-5,j+1)&
           -7.5D0*(4.D0*Nh(i-4,j-5)-30.D0*Nh(i-4,j-4)+100.D0*Nh(i-4,j-3)-200.D0*Nh(i-4,j-2)+300.D0*Nh(i-4,j-1)&
            -154.D0*Nh(i-4,j)-20.D0*Nh(i-4,j+1))&
           +25.D0*(4.D0*Nh(i-3,j-5)-30.D0*Nh(i-3,j-4)+100.D0*Nh(i-3,j-3)-200.D0*Nh(i-3,j-2)+300.D0*Nh(i-3,j-1)&
            -154.D0*Nh(i-3,j)-20.D0*Nh(i-3,j+1))&
           -50.D0*(4.D0*Nh(i-2,j-5)-30.D0*Nh(i-2,j-4)+100.D0*Nh(i-2,j-3)-200.D0*Nh(i-2,j-2)+300.D0*Nh(i-2,j-1)&
            -154.D0*Nh(i-2,j)-20.D0*Nh(i-2,j+1))&
           +75.D0*(4.D0*Nh(i-1,j-5)-30.D0*Nh(i-1,j-4)+100.D0*Nh(i-1,j-3)-200.D0*Nh(i-1,j-2)+300.D0*Nh(i-1,j-1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j+1))&
           -38.5D0*(4.D0*Nh(i,j-5)-30.D0*Nh(i,j-4)+100.D0*Nh(i,j-3)-200.D0*Nh(i,j-2)+300.D0*Nh(i,j-1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j+1))&
           -5.D0*(4.D0*Nh(i+1,j-5)-30.D0*Nh(i+1,j-4)+100.D0*Nh(i+1,j-3)-200.D0*Nh(i+1,j-2)+300.D0*Nh(i+1,j-1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j+1)))/(3600.D0*hx*hy)
!
!* x,y - centered,forward
j=2
DO i=3,Nx-2
   dxk1(i,j)=(k1(i-2,j)-8.D0*k1(i-1,j)+8.D0*k1(i+1,j)-k1(i+2,j))/(12.D0*hx)
   dyk1(i,j)=-(-2.D0*k1(i,j+5)+15.D0*k1(i,j+4)-50.D0*k1(i,j+3)+100.D0*k1(i,j+2)-150.D0*k1(i,j+1)+77.D0*k1(i,j)+10.D0*k1(i,j-1))&
             /(60.D0*hy)
   !
   dxk2(i,j)=(k2(i-2,j)-8.D0*k2(i-1,j)+8.D0*k2(i+1,j)-k2(i+2,j))/(12.D0*hx)
   dyk2(i,j)=-(-2.D0*k2(i,j+5)+15.D0*k2(i,j+4)-50.D0*k2(i,j+3)+100.D0*k2(i,j+2)-150.D0*k2(i,j+1)+77.D0*k2(i,j)+10.D0*k2(i,j-1))&
             /(60.D0*hy)
   !
   dxNh(i,j)=(Nh(i-2,j)-8.D0*Nh(i-1,j)+8.D0*Nh(i+1,j)-Nh(i+2,j))/(12.D0*hx)
   dyNh(i,j)=-(-2.D0*Nh(i,j+5)+15.D0*Nh(i,j+4)-50.D0*Nh(i,j+3)+100.D0*Nh(i,j+2)-150.D0*Nh(i,j+1)+77.D0*Nh(i,j)+10.D0*Nh(i,j-1))&
             /(60.D0*hy)
   dxxNh(i,j)=(-Nh(i-2,j)+16.D0*Nh(i-1,j)-30.D0*Nh(i,j)+16.D0*Nh(i+1,j)-Nh(i+2,j))/(12.D0*hx*hx)
   dyyNh(i,j)=-(13.D0*Nh(i,j+5)-93.D0*Nh(i,j+4)+285.D0*Nh(i,j+3)-470.D0*Nh(i,j+2)+255.D0*Nh(i,j+1)+147.D0*Nh(i,j)-137.D0*Nh(i,j-1))&
              /(180.D0*hy*hy)
   !
   dxKK(i,j)=(KK(i-2,j)-8.D0*KK(i-1,j)+8.D0*KK(i+1,j)-KK(i+2,j))/(12.D0*hx)
   dyKK(i,j)=-(-2.D0*KK(i,j+5)+15.D0*KK(i,j+4)-50.D0*KK(i,j+3)+100.D0*KK(i,j+2)-150.D0*KK(i,j+1)+77.D0*KK(i,j)+10.D0*KK(i,j-1))&
             /(60.D0*hy)
END DO
!
DO i=4,Nx-3
   dxyNh(i,j)=-(2.D0*Nh(i-3,j+5)-18.D0*Nh(i-2,j+5)+90.D0*Nh(i-1,j+5)-90.D0*Nh(i+1,j+5)+18.D0*Nh(i+2,j+5)-2.D0*Nh(i+3,j+5)&
              -7.5D0*(2.D0*Nh(i-3,j+4)-18.D0*Nh(i-2,j+4)+90.D0*Nh(i-1,j+4)-90.D0*Nh(i+1,j+4)+18.D0*Nh(i+2,j+4)-2.D0*Nh(i+3,j+4))&
              +25.D0*(2.D0*Nh(i-3,j+3)-18.D0*Nh(i-2,j+3)+90.D0*Nh(i-1,j+3)-90.D0*Nh(i+1,j+3)+18.D0*Nh(i+2,j+3)-2.D0*Nh(i+3,j+3))&
              -50.D0*(2.D0*Nh(i-3,j+2)-18.D0*Nh(i-2,j+2)+90.D0*Nh(i-1,j+2)-90.D0*Nh(i+1,j+2)+18.D0*Nh(i+2,j+2)-2.D0*Nh(i+3,j+2))&
              +75.D0*(2.D0*Nh(i-3,j+1)-18.D0*Nh(i-2,j+1)+90.D0*Nh(i-1,j+1)-90.D0*Nh(i+1,j+1)+18.D0*Nh(i+2,j+1)-2.D0*Nh(i+3,j+1))&
              -38.5D0*(2.D0*Nh(i-3,j)-18.D0*Nh(i-2,j)+90.D0*Nh(i-1,j)-90.D0*Nh(i+1,j)+18.D0*Nh(i+2,j)-2.D0*Nh(i+3,j))&
              -5.D0*(2.D0*Nh(i-3,j-1)-18.D0*Nh(i-2,j-1)+90.D0*Nh(i-1,j-1)-90.D0*Nh(i+1,j-1)+18.D0*Nh(i+2,j-1)-2.D0*Nh(i+3,j-1)))&
              /(3600.D0*hx*hy)
END DO
!
i=3
dxyNh(i,j)=(4.D0*Nh(i+5,j+5)-30.D0*Nh(i+5,j+4)+100.D0*Nh(i+5,j+3)-200.D0*Nh(i+5,j+2)+300.D0*Nh(i+5,j+1)&
            -154.D0*Nh(i+5,j)-20.D0*Nh(i+5,j-1)&
           -7.5D0*(4.D0*Nh(i+4,j+5)-30.D0*Nh(i+4,j+4)+100.D0*Nh(i+4,j+3)-200.D0*Nh(i+4,j+2)+300.D0*Nh(i+4,j+1)&
            -154.D0*Nh(i+4,j)-20.D0*Nh(i+4,j-1))&
           +25.D0*(4.D0*Nh(i+3,j+5)-30.D0*Nh(i+3,j+4)+100.D0*Nh(i+3,j+3)-200.D0*Nh(i+3,j+2)+300.D0*Nh(i+3,j+1)&
            -154.D0*Nh(i+3,j)-20.D0*Nh(i+3,j-1))&
           -50.D0*(4.D0*Nh(i+2,j+5)-30.D0*Nh(i+2,j+4)+100.D0*Nh(i+2,j+3)-200.D0*Nh(i+2,j+2)+300.D0*Nh(i+2,j+1)&
            -154.D0*Nh(i+2,j)-20.D0*Nh(i+2,j-1))&
           +75.D0*(4.D0*Nh(i+1,j+5)-30.D0*Nh(i+1,j+4)+100.D0*Nh(i+1,j+3)-200.D0*Nh(i+1,j+2)+300.D0*Nh(i+1,j+1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j-1))&
           -38.5D0*(4.D0*Nh(i,j+5)-30.D0*Nh(i,j+4)+100.D0*Nh(i,j+3)-200.D0*Nh(i,j+2)+300.D0*Nh(i,j+1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j-1))&
           -5.D0*(4.D0*Nh(i-1,j+5)-30.D0*Nh(i-1,j+4)+100.D0*Nh(i-1,j+3)-200.D0*Nh(i-1,j+2)+300.D0*Nh(i-1,j+1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j-1)))/(3600.D0*hx*hy)
!
i=Nx-2
dxyNh(i,j)=-(4.D0*Nh(i-5,j+5)-30.D0*Nh(i-5,j+4)+100.D0*Nh(i-5,j+3)-200.D0*Nh(i-5,j+2)+300.D0*Nh(i-5,j+1)&
            -154.D0*Nh(i-5,j)-20.D0*Nh(i-5,j-1)&
           -7.5D0*(4.D0*Nh(i-4,j+5)-30.D0*Nh(i-4,j+4)+100.D0*Nh(i-4,j+3)-200.D0*Nh(i-4,j+2)+300.D0*Nh(i-4,j+1)&
            -154.D0*Nh(i-4,j)-20.D0*Nh(i-4,j-1))&
           +25.D0*(4.D0*Nh(i-3,j+5)-30.D0*Nh(i-3,j+4)+100.D0*Nh(i-3,j+3)-200.D0*Nh(i-3,j+2)+300.D0*Nh(i-3,j+1)&
            -154.D0*Nh(i-3,j)-20.D0*Nh(i-3,j-1))&
           -50.D0*(4.D0*Nh(i-2,j+5)-30.D0*Nh(i-2,j+4)+100.D0*Nh(i-2,j+3)-200.D0*Nh(i-2,j+2)+300.D0*Nh(i-2,j+1)&
            -154.D0*Nh(i-2,j)-20.D0*Nh(i-2,j-1))&
           +75.D0*(4.D0*Nh(i-1,j+5)-30.D0*Nh(i-1,j+4)+100.D0*Nh(i-1,j+3)-200.D0*Nh(i-1,j+2)+300.D0*Nh(i-1,j+1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j-1))&
           -38.5D0*(4.D0*Nh(i,j+5)-30.D0*Nh(i,j+4)+100.D0*Nh(i,j+3)-200.D0*Nh(i,j+2)+300.D0*Nh(i,j+1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j-1))&
           -5.D0*(4.D0*Nh(i+1,j+5)-30.D0*Nh(i+1,j+4)+100.D0*Nh(i+1,j+3)-200.D0*Nh(i+1,j+2)+300.D0*Nh(i+1,j+1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j-1)))/(3600.D0*hx*hy)
!
!* x,y - centered,back
j=Ny-1
DO i=3,Nx-2
   dxk1(i,j)=(k1(i-2,j)-8.D0*k1(i-1,j)+8.D0*k1(i+1,j)-k1(i+2,j))/(12.D0*hx)
   dyk1(i,j)=(-2.D0*k1(i,j-5)+15.D0*k1(i,j-4)-50.D0*k1(i,j-3)+100.D0*k1(i,j-2)-150.D0*k1(i,j-1)+77.D0*k1(i,j)+10.D0*k1(i,j+1))&
             /(60.D0*hy)
   !
   dxk2(i,j)=(k2(i-2,j)-8.D0*k2(i-1,j)+8.D0*k2(i+1,j)-k2(i+2,j))/(12.D0*hx)
   dyk2(i,j)=(-2.D0*k2(i,j-5)+15.D0*k2(i,j-4)-50.D0*k2(i,j-3)+100.D0*k2(i,j-2)-150.D0*k2(i,j-1)+77.D0*k2(i,j)+10.D0*k2(i,j+1))&
             /(60.D0*hy)
   !
   dxNh(i,j)=(Nh(i-2,j)-8.D0*Nh(i-1,j)+8.D0*Nh(i+1,j)-Nh(i+2,j))/(12.D0*hx)
   dyNh(i,j)=(-2.D0*Nh(i,j-5)+15.D0*Nh(i,j-4)-50.D0*Nh(i,j-3)+100.D0*Nh(i,j-2)-150.D0*Nh(i,j-1)+77.D0*Nh(i,j)+10.D0*Nh(i,j+1))&
             /(60.D0*hy)
   dxxNh(i,j)=(-Nh(i-2,j)+16.D0*Nh(i-1,j)-30.D0*Nh(i,j)+16.D0*Nh(i+1,j)-Nh(i+2,j))/(12.D0*hx*hx)
   dyyNh(i,j)=-(13.D0*Nh(i,j-5)-93.D0*Nh(i,j-4)+285.D0*Nh(i,j-3)-470.D0*Nh(i,j-2)+255.D0*Nh(i,j-1)+147.D0*Nh(i,j)-137.D0*Nh(i,j+1))&
              /(180.D0*hy*hy)
   !
   dxKK(i,j)=(KK(i-2,j)-8.D0*KK(i-1,j)+8.D0*KK(i+1,j)-KK(i+2,j))/(12.D0*hx)
   dyKK(i,j)=(-2.D0*KK(i,j-5)+15.D0*KK(i,j-4)-50.D0*KK(i,j-3)+100.D0*KK(i,j-2)-150.D0*KK(i,j-1)+77.D0*KK(i,j)+10.D0*KK(i,j+1))&
             /(60.D0*hy)
END DO
!
DO i=4,Nx-3
   dxyNh(i,j)=(2.D0*Nh(i-3,j-5)-18.D0*Nh(i-2,j-5)+90.D0*Nh(i-1,j-5)-90.D0*Nh(i+1,j-5)+18.D0*Nh(i+2,j-5)-2.D0*Nh(i+3,j-5)&
              -7.5D0*(2.D0*Nh(i-3,j-4)-18.D0*Nh(i-2,j-4)+90.D0*Nh(i-1,j-4)-90.D0*Nh(i+1,j-4)+18.D0*Nh(i+2,j-4)-2.D0*Nh(i+3,j-4))&
              +25.D0*(2.D0*Nh(i-3,j-3)-18.D0*Nh(i-2,j-3)+90.D0*Nh(i-1,j-3)-90.D0*Nh(i+1,j-3)+18.D0*Nh(i+2,j-3)-2.D0*Nh(i+3,j-3))&
              -50.D0*(2.D0*Nh(i-3,j-2)-18.D0*Nh(i-2,j-2)+90.D0*Nh(i-1,j-2)-90.D0*Nh(i+1,j-2)+18.D0*Nh(i+2,j-2)-2.D0*Nh(i+3,j-2))&
              +75.D0*(2.D0*Nh(i-3,j-1)-18.D0*Nh(i-2,j-1)+90.D0*Nh(i-1,j-1)-90.D0*Nh(i+1,j-1)+18.D0*Nh(i+2,j-1)-2.D0*Nh(i+3,j-1))&
              -38.5D0*(2.D0*Nh(i-3,j)-18.D0*Nh(i-2,j)+90.D0*Nh(i-1,j)-90.D0*Nh(i+1,j)+18.D0*Nh(i+2,j)-2.D0*Nh(i+3,j))&
              -5.D0*(2.D0*Nh(i-3,j+1)-18.D0*Nh(i-2,j+1)+90.D0*Nh(i-1,j+1)-90.D0*Nh(i+1,j+1)+18.D0*Nh(i+2,j+1)-2.D0*Nh(i+3,j+1)))&
              /(3600.D0*hx*hy)
END DO
!
i=3
dxyNh(i,j)=-(4.D0*Nh(i+5,j-5)-30.D0*Nh(i+5,j-4)+100.D0*Nh(i+5,j-3)-200.D0*Nh(i+5,j-2)+300.D0*Nh(i+5,j-1)&
            -154.D0*Nh(i+5,j)-20.D0*Nh(i+5,j+1)&
           -7.5D0*(4.D0*Nh(i+4,j-5)-30.D0*Nh(i+4,j-4)+100.D0*Nh(i+4,j-3)-200.D0*Nh(i+4,j-2)+300.D0*Nh(i+4,j-1)&
            -154.D0*Nh(i+4,j)-20.D0*Nh(i+4,j+1))&
           +25.D0*(4.D0*Nh(i+3,j-5)-30.D0*Nh(i+3,j-4)+100.D0*Nh(i+3,j-3)-200.D0*Nh(i+3,j-2)+300.D0*Nh(i+3,j-1)&
            -154.D0*Nh(i+3,j)-20.D0*Nh(i+3,j+1))&
           -50.D0*(4.D0*Nh(i+2,j-5)-30.D0*Nh(i+2,j-4)+100.D0*Nh(i+2,j-3)-200.D0*Nh(i+2,j-2)+300.D0*Nh(i+2,j-1)&
            -154.D0*Nh(i+2,j)-20.D0*Nh(i+2,j+1))&
           +75.D0*(4.D0*Nh(i+1,j-5)-30.D0*Nh(i+1,j-4)+100.D0*Nh(i+1,j-3)-200.D0*Nh(i+1,j-2)+300.D0*Nh(i+1,j-1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j+1))&
           -38.5D0*(4.D0*Nh(i,j-5)-30.D0*Nh(i,j-4)+100.D0*Nh(i,j-3)-200.D0*Nh(i,j-2)+300.D0*Nh(i,j-1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j+1))&
           -5.D0*(4.D0*Nh(i-1,j-5)-30.D0*Nh(i-1,j-4)+100.D0*Nh(i-1,j-3)-200.D0*Nh(i-1,j-2)+300.D0*Nh(i-1,j-1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j+1)))/(3600.D0*hx*hy)
!
i=Nx-2
dxyNh(i,j)=(4.D0*Nh(i-5,j-5)-30.D0*Nh(i-5,j-4)+100.D0*Nh(i-5,j-3)-200.D0*Nh(i-5,j-2)+300.D0*Nh(i-5,j-1)&
            -154.D0*Nh(i-5,j)-20.D0*Nh(i-5,j+1)&
           -7.5D0*(4.D0*Nh(i-4,j-5)-30.D0*Nh(i-4,j-4)+100.D0*Nh(i-4,j-3)-200.D0*Nh(i-4,j-2)+300.D0*Nh(i-4,j-1)&
            -154.D0*Nh(i-4,j)-20.D0*Nh(i-4,j+1))&
           +25.D0*(4.D0*Nh(i-3,j-5)-30.D0*Nh(i-3,j-4)+100.D0*Nh(i-3,j-3)-200.D0*Nh(i-3,j-2)+300.D0*Nh(i-3,j-1)&
            -154.D0*Nh(i-3,j)-20.D0*Nh(i-3,j+1))&
           -50.D0*(4.D0*Nh(i-2,j-5)-30.D0*Nh(i-2,j-4)+100.D0*Nh(i-2,j-3)-200.D0*Nh(i-2,j-2)+300.D0*Nh(i-2,j-1)&
            -154.D0*Nh(i-2,j)-20.D0*Nh(i-2,j+1))&
           +75.D0*(4.D0*Nh(i-1,j-5)-30.D0*Nh(i-1,j-4)+100.D0*Nh(i-1,j-3)-200.D0*Nh(i-1,j-2)+300.D0*Nh(i-1,j-1)&
            -154.D0*Nh(i-1,j)-20.D0*Nh(i-1,j+1))&
           -38.5D0*(4.D0*Nh(i,j-5)-30.D0*Nh(i,j-4)+100.D0*Nh(i,j-3)-200.D0*Nh(i,j-2)+300.D0*Nh(i,j-1)&
            -154.D0*Nh(i,j)-20.D0*Nh(i,j+1))&
           -5.D0*(4.D0*Nh(i+1,j-5)-30.D0*Nh(i+1,j-4)+100.D0*Nh(i+1,j-3)-200.D0*Nh(i+1,j-2)+300.D0*Nh(i+1,j-1)&
            -154.D0*Nh(i+1,j)-20.D0*Nh(i+1,j+1)))/(3600.D0*hx*hy)

END SUBROUTINE Pochodne

END MODULE module111Pochodne
