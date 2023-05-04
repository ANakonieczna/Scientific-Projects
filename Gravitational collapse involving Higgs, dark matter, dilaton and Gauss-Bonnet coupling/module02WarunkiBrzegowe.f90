MODULE moduleWarunkiBrzegowe

CONTAINS
SUBROUTINE WarunkiBrzegowe(h0U,&
 rprev,gprev,h,a,f,Q,&
 !OUT:
 r,g)
USE modulerhs
IMPLICIT NONE
REAL(8),INTENT(IN) :: h0U
REAL(8),INTENT(IN) :: rprev,gprev,h,a,f,Q
REAL(8),INTENT(OUT) :: r,g
!
REAL(8) :: k1g,k2g,r_mod,g_mod

!*** z rPOu ***
r=rprev+h0U*f

!*** z gPOu ***
!* RK2-k1 *
k1g=rhsgf(h,a,rprev,f,gprev,Q)

!* do RK2-k2 *
!******* fkcje mod *******
r_mod=rprev+0.5D0*h0U*f
g_mod=gprev+0.5D0*h0U*k1g
!
!* RK2-k2 *
k2g=rhsgf(h,a,r_mod,f,g_mod,Q)

g=gprev+h0U*k2g

END SUBROUTINE WarunkiBrzegowe

END MODULE moduleWarunkiBrzegowe
