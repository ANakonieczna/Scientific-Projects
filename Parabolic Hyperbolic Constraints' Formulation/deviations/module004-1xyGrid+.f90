MODULE module004xyGrid

CONTAINS
SUBROUTINE xyGrid(x,y)
!
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy
INTEGER :: Nx,Ny
REAL(8) :: p2
COMMON /PARAMS/ M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy,Nx,Ny,p2
!*************************
REAL(8),DIMENSION(:),INTENT(OUT) :: x,y
INTEGER :: i,j

!******* siatka (x,y) *******
x(1)=-U !x(Nx)
DO i=2,Nx
   x(i)=x(i-1)+hx
END DO
y(1)=-U !y(Ny)
DO j=2,Ny
   y(j)=y(j-1)+hy
END DO

END SUBROUTINE xyGrid

END MODULE module004xyGrid
