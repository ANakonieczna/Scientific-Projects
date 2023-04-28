MODULE module222k1234

CONTAINS
SUBROUTINE k1234(H1,H1t,H1tt,H1tx,H1ty,H1tz,H1x,H1xx,H1xy,H1xz,H1y,H1yy,H1yz,H1z,H1zz,&
 H2,H2t,H2tt,H2tx,H2ty,H2tz,H2x,H2xx,H2xy,H2xz,H2y,H2yy,H2yz,H2z,H2zz,H1H2,&
 !
 l11,l11t,l11tt,l11tx,l11ty,l11tz,l11x,l11xx,l11xy,l11xz,l11y,l11yy,l11yz,l11z,l11zz,&
 l12,l12t,l12tt,l12tx,l12ty,l12tz,l12x,l12xx,l12xy,l12xz,l12y,l12yy,l12yz,l12z,l12zz,&
 l13,l13t,l13tt,l13tx,l13ty,l13tz,l13x,l13xx,l13xy,l13xz,l13y,l13yy,l13yz,l13z,l13zz,&
 l21,l21t,l21tt,l21tx,l21ty,l21tz,l21x,l21xx,l21xy,l21xz,l21y,l21yy,l21yz,l21z,l21zz,&
 l22,l22t,l22tt,l22tx,l22ty,l22tz,l22x,l22xx,l22xy,l22xz,l22y,l22yy,l22yz,l22z,l22zz,&
 l23,l23t,l23tt,l23tx,l23ty,l23tz,l23x,l23xx,l23xy,l23xz,l23y,l23yy,l23yz,l23z,l23zz,&
 !
 l11n1,l21n2,l11n12,l21n22,l112,l122,l132,l212,l222,l232,&
 !
 f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,&
 f31,f32,f33,f34,f35,f36,f37,f38,f39,f40,f41,f42,f43,f44,f45,f46,f47,f48,f49,f50,f51,f52,f53,f54,f55,f56,f57,f58,f59,f60,&
 f61,f62,f63,f64,f65,f66,f67,f68,f69,f70,f71,f72,f73,f74,f75,f76,f77,f78,f79,f80,f81,f82,f83,f84,f85,f86,f87,f88,f89,f90,&
 f91,f92,f93,f94,f95,f96,f97,f98,f99,f100,f101,f102,f103,f104,f105,f106,f107,f108,f109,f110,&
 f111,f112,f113,f114,f115,f116,f117,f118,f119,&
 !
 ff1,ff2,ff3,ff4,ff5,ff6,ff7,ff8,ff9,ff10,ff11,ff12,ff13,ff14,ff15,ff16,ff17,ff18,ff19,ff20,&
 ff21,ff22,ff23,ff24,ff25,ff26,ff27,ff28,ff29,ff30,ff31,ff32,ff33,ff34,ff35,&
 !
 G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,k1,k2,Nh,KK,dxNh,dyNh,dxKK,dyKK,dxkappa,dykappa,&
 dxk1,dyk1,dxk2,dyk2,dxxNh,dyyNh,dxyNh,Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh,&
 !OUT:
 kKK,kk1,kk2,kNh)
!
USE module222rhsRK4
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy
INTEGER :: Nx,Ny
REAL(8) :: p2
COMMON /PARAMS/ M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy,Nx,Ny,p2
!*************************
REAL(8),DIMENSION(:,:),INTENT(IN) :: H1,H1t,H1tt,H1tx,H1ty,H1tz,H1x,H1xx,H1xy,H1xz,H1y,H1yy,H1yz,H1z,H1zz
REAL(8),DIMENSION(:,:),INTENT(IN) :: H2,H2t,H2tt,H2tx,H2ty,H2tz,H2x,H2xx,H2xy,H2xz,H2y,H2yy,H2yz,H2z,H2zz,H1H2
REAL(8),DIMENSION(:,:),INTENT(IN) :: l11,l11t,l11tt,l11tx,l11ty,l11tz,l11x,l11xx,l11xy,l11xz,l11y,l11yy,l11yz,l11z,l11zz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l12,l12t,l12tt,l12tx,l12ty,l12tz,l12x,l12xx,l12xy,l12xz,l12y,l12yy,l12yz,l12z,l12zz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l13,l13t,l13tt,l13tx,l13ty,l13tz,l13x,l13xx,l13xy,l13xz,l13y,l13yy,l13yz,l13z,l13zz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l21,l21t,l21tt,l21tx,l21ty,l21tz,l21x,l21xx,l21xy,l21xz,l21y,l21yy,l21yz,l21z,l21zz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l22,l22t,l22tt,l22tx,l22ty,l22tz,l22x,l22xx,l22xy,l22xz,l22y,l22yy,l22yz,l22z,l22zz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l23,l23t,l23tt,l23tx,l23ty,l23tz,l23x,l23xx,l23xy,l23xz,l23y,l23yy,l23yz,l23z,l23zz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l11n1,l21n2,l11n12,l21n22,l112,l122,l132,l212,l222,l232
REAL(8),DIMENSION(:,:),INTENT(IN) :: f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20
REAL(8),DIMENSION(:,:),INTENT(IN) :: f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36,f37,f38,f39,f40
REAL(8),DIMENSION(:,:),INTENT(IN) :: f41,f42,f43,f44,f45,f46,f47,f48,f49,f50,f51,f52,f53,f54,f55,f56,f57,f58,f59,f60
REAL(8),DIMENSION(:,:),INTENT(IN) :: f61,f62,f63,f64,f65,f66,f67,f68,f69,f70,f71,f72,f73,f74,f75,f76,f77,f78,f79,f80
REAL(8),DIMENSION(:,:),INTENT(IN) :: f81,f82,f83,f84,f85,f86,f87,f88,f89,f90,f91,f92,f93,f94,f95,f96,f97,f98,f99,f100
REAL(8),DIMENSION(:,:),INTENT(IN) :: f101,f102,f103,f104,f105,f106,f107,f108,f109,f110,f111,f112,f113,f114,f115
REAL(8),DIMENSION(:,:),INTENT(IN) :: f116,f117,f118,f119
REAL(8),DIMENSION(:,:),INTENT(IN) :: ff1,ff2,ff3,ff4,ff5,ff6,ff7,ff8,ff9,ff10,ff11,ff12,ff13,ff14,ff15,ff16,ff17,ff18,ff19,ff20
REAL(8),DIMENSION(:,:),INTENT(IN) :: ff21,ff22,ff23,ff24,ff25,ff26,ff27,ff28,ff29,ff30,ff31,ff32,ff33,ff34,ff35
REAL(8),DIMENSION(:,:),INTENT(IN) :: G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar
REAL(8),DIMENSION(:,:),INTENT(IN) :: k1,k2,Nh,KK,dxNh,dyNh,dxKK,dyKK,dxkappa,dykappa
REAL(8),DIMENSION(:,:),INTENT(IN) :: dxk1,dyk1,dxk2,dyk2,dxxNh,dyyNh,dxyNh
REAL(8),DIMENSION(:,:),INTENT(IN) :: Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh
REAL(8),DIMENSION(:,:),INTENT(OUT) :: kKK,kk1,kk2,kNh
INTEGER :: i,j

!* RK4-k
DO i=2,Nx-1
 DO j=2,Ny-1
    kKK(i,j)=rhsKK(G1(i,j),G2(i,j),G3(i,j),G4(i,j),G5(i,j),G6(i,j),G7(i,j),G8(i,j),dxG1(i,j),dyG1(i,j),dxG2(i,j),dyG2(i,j),&
     a(i,j),d(i,j),kappa(i,j),Kstar(i,j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),dxNh(i,j),dyNh(i,j),dxKK(i,j),dyKK(i,j),&
     dxkappa(i,j),dykappa(i,j),dxk1(i,j),dyk1(i,j),dxk2(i,j),dyk2(i,j),&
     Ak1(i,j),Ak2(i,j),ANh(i,j),AKK(i,j),AdxNh(i,j),AdyNh(i,j),AdxKK(i,j),AdyKK(i,j),Adxk1(i,j),Adyk1(i,j),Adxk2(i,j),Adyk2(i,j),&
     AdxxNh(i,j),AdyyNh(i,j),AdxyNh(i,j))
    !
    kk1(i,j)=rhsk1(H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
     H1y(i,j),H1yy(i,j),H1yz(i,j),H1z(i,j),H1zz(i,j),&
     H2(i,j),H2t(i,j),H2tt(i,j),H2tx(i,j),H2ty(i,j),H2tz(i,j),H2x(i,j),H2xx(i,j),H2xy(i,j),H2xz(i,j),&
     H2y(i,j),H2yy(i,j),H2yz(i,j),H2z(i,j),H2zz(i,j),H1H2(i,j),&
     !
     l11(i,j),l11t(i,j),l11tt(i,j),l11tx(i,j),l11ty(i,j),l11tz(i,j),l11x(i,j),l11xx(i,j),l11xy(i,j),l11xz(i,j),&
     l11y(i,j),l11yy(i,j),l11yz(i,j),l11z(i,j),l11zz(i,j),&
     l12(i,j),l12t(i,j),l12tt(i,j),l12tx(i,j),l12ty(i,j),l12tz(i,j),l12x(i,j),l12xx(i,j),l12xy(i,j),l12xz(i,j),&
     l12y(i,j),l12yy(i,j),l12yz(i,j),l12z(i,j),l12zz(i,j),&
     l13(i,j),l13t(i,j),l13tt(i,j),l13tx(i,j),l13ty(i,j),l13tz(i,j),l13x(i,j),l13xx(i,j),l13xy(i,j),l13xz(i,j),&
     l13y(i,j),l13yy(i,j),l13yz(i,j),l13z(i,j),l13zz(i,j),&
     l21(i,j),l21t(i,j),l21tt(i,j),l21tx(i,j),l21ty(i,j),l21tz(i,j),l21x(i,j),l21xx(i,j),l21xy(i,j),l21xz(i,j),&
     l21y(i,j),l21yy(i,j),l21yz(i,j),l21z(i,j),l21zz(i,j),&
     l22(i,j),l22t(i,j),l22tt(i,j),l22tx(i,j),l22ty(i,j),l22tz(i,j),l22x(i,j),l22xx(i,j),l22xy(i,j),l22xz(i,j),&
     l22y(i,j),l22yy(i,j),l22yz(i,j),l22z(i,j),l22zz(i,j),&
     l23(i,j),l23t(i,j),l23tt(i,j),l23tx(i,j),l23ty(i,j),l23tz(i,j),l23x(i,j),l23xx(i,j),l23xy(i,j),l23xz(i,j),&
     l23y(i,j),l23yy(i,j),l23yz(i,j),l23z(i,j),l23zz(i,j),&
     !
     l11n1(i,j),l21n2(i,j),l11n12(i,j),l21n22(i,j),l112(i,j),l122(i,j),l132(i,j),l212(i,j),l222(i,j),l232(i,j),&
     !
     f1(i,j),f2(i,j),f3(i,j),f4(i,j),f5(i,j),f6(i,j),f7(i,j),f8(i,j),f9(i,j),f10(i,j),&
     f11(i,j),f12(i,j),f13(i,j),f14(i,j),f15(i,j),f16(i,j),f17(i,j),f18(i,j),f19(i,j),f20(i,j),&
     f21(i,j),f22(i,j),f23(i,j),f24(i,j),f25(i,j),f26(i,j),f27(i,j),f28(i,j),f29(i,j),f30(i,j),&
     f31(i,j),f32(i,j),f33(i,j),f34(i,j),f35(i,j),f36(i,j),f37(i,j),f38(i,j),f39(i,j),f40(i,j),&
     f41(i,j),f42(i,j),f43(i,j),f44(i,j),f45(i,j),f46(i,j),f47(i,j),f48(i,j),f49(i,j),f50(i,j),&
     f51(i,j),f52(i,j),f53(i,j),f54(i,j),f55(i,j),f56(i,j),f57(i,j),f58(i,j),f59(i,j),f60(i,j),&
     f61(i,j),f62(i,j),f63(i,j),f64(i,j),f65(i,j),f66(i,j),f67(i,j),f68(i,j),f69(i,j),f70(i,j),&
     f71(i,j),f72(i,j),f73(i,j),f74(i,j),f75(i,j),f76(i,j),f77(i,j),f78(i,j),f79(i,j),f80(i,j),&
     f81(i,j),f82(i,j),f83(i,j),f84(i,j),f85(i,j),f86(i,j),f87(i,j),f88(i,j),f89(i,j),f90(i,j),&
     f91(i,j),f92(i,j),f93(i,j),f94(i,j),f95(i,j),f96(i,j),f97(i,j),f98(i,j),f99(i,j),f100(i,j),&
     f101(i,j),f102(i,j),f103(i,j),f104(i,j),f105(i,j),f106(i,j),f107(i,j),f108(i,j),f109(i,j),f110(i,j),&
     f111(i,j),f112(i,j),f113(i,j),f114(i,j),f115(i,j),f116(i,j),f117(i,j),f118(i,j),f119(i,j),&
     !
     ff1(i,j),ff2(i,j),ff3(i,j),ff4(i,j),ff5(i,j),ff6(i,j),ff7(i,j),ff8(i,j),ff9(i,j),ff10(i,j),&
     ff11(i,j),ff12(i,j),ff13(i,j),ff14(i,j),ff15(i,j),ff16(i,j),ff17(i,j),ff18(i,j),ff19(i,j),ff20(i,j),&
     ff21(i,j),ff22(i,j),ff23(i,j),ff24(i,j),ff25(i,j),ff26(i,j),ff27(i,j),ff28(i,j),ff29(i,j),ff30(i,j),&
     ff31(i,j),ff32(i,j),ff33(i,j),ff34(i,j),ff35(i,j),&
     !
     G1(i,j),G2(i,j),G3(i,j),G4(i,j),G5(i,j),G6(i,j),G7(i,j),G8(i,j),dxG1(i,j),dyG1(i,j),dxG2(i,j),dyG2(i,j),&
     a(i,j),d(i,j),kappa(i,j),Kstar(i,j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),dxNh(i,j),dyNh(i,j),dxKK(i,j),dyKK(i,j),&
     dxkappa(i,j),dykappa(i,j),dxk1(i,j),dyk1(i,j),dxk2(i,j),dyk2(i,j),&
     Ak1(i,j),Ak2(i,j),ANh(i,j),AKK(i,j),AdxNh(i,j),AdyNh(i,j),AdxKK(i,j),AdyKK(i,j),Adxk1(i,j),Adyk1(i,j),Adxk2(i,j),Adyk2(i,j),&
     AdxxNh(i,j),AdyyNh(i,j),AdxyNh(i,j))
    !
    kk2(i,j)=rhsk2(H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
     H1y(i,j),H1yy(i,j),H1yz(i,j),H1z(i,j),H1zz(i,j),&
     H2(i,j),H2t(i,j),H2tt(i,j),H2tx(i,j),H2ty(i,j),H2tz(i,j),H2x(i,j),H2xx(i,j),H2xy(i,j),H2xz(i,j),&
     H2y(i,j),H2yy(i,j),H2yz(i,j),H2z(i,j),H2zz(i,j),H1H2(i,j),&
     !
     l11(i,j),l11t(i,j),l11tt(i,j),l11tx(i,j),l11ty(i,j),l11tz(i,j),l11x(i,j),l11xx(i,j),l11xy(i,j),l11xz(i,j),&
     l11y(i,j),l11yy(i,j),l11yz(i,j),l11z(i,j),l11zz(i,j),&
     l12(i,j),l12t(i,j),l12tt(i,j),l12tx(i,j),l12ty(i,j),l12tz(i,j),l12x(i,j),l12xx(i,j),l12xy(i,j),l12xz(i,j),&
     l12y(i,j),l12yy(i,j),l12yz(i,j),l12z(i,j),l12zz(i,j),&
     l13(i,j),l13t(i,j),l13tt(i,j),l13tx(i,j),l13ty(i,j),l13tz(i,j),l13x(i,j),l13xx(i,j),l13xy(i,j),l13xz(i,j),&
     l13y(i,j),l13yy(i,j),l13yz(i,j),l13z(i,j),l13zz(i,j),&
     l21(i,j),l21t(i,j),l21tt(i,j),l21tx(i,j),l21ty(i,j),l21tz(i,j),l21x(i,j),l21xx(i,j),l21xy(i,j),l21xz(i,j),&
     l21y(i,j),l21yy(i,j),l21yz(i,j),l21z(i,j),l21zz(i,j),&
     l22(i,j),l22t(i,j),l22tt(i,j),l22tx(i,j),l22ty(i,j),l22tz(i,j),l22x(i,j),l22xx(i,j),l22xy(i,j),l22xz(i,j),&
     l22y(i,j),l22yy(i,j),l22yz(i,j),l22z(i,j),l22zz(i,j),&
     l23(i,j),l23t(i,j),l23tt(i,j),l23tx(i,j),l23ty(i,j),l23tz(i,j),l23x(i,j),l23xx(i,j),l23xy(i,j),l23xz(i,j),&
     l23y(i,j),l23yy(i,j),l23yz(i,j),l23z(i,j),l23zz(i,j),&
     !
     l11n1(i,j),l21n2(i,j),l11n12(i,j),l21n22(i,j),l112(i,j),l122(i,j),l132(i,j),l212(i,j),l222(i,j),l232(i,j),&
     !
     f1(i,j),f2(i,j),f3(i,j),f4(i,j),f5(i,j),f6(i,j),f7(i,j),f8(i,j),f9(i,j),f10(i,j),&
     f11(i,j),f12(i,j),f13(i,j),f14(i,j),f15(i,j),f16(i,j),f17(i,j),f18(i,j),f19(i,j),f20(i,j),&
     f21(i,j),f22(i,j),f23(i,j),f24(i,j),f25(i,j),f26(i,j),f27(i,j),f28(i,j),f29(i,j),f30(i,j),&
     f31(i,j),f32(i,j),f33(i,j),f34(i,j),f35(i,j),f36(i,j),f37(i,j),f38(i,j),f39(i,j),f40(i,j),&
     f41(i,j),f42(i,j),f43(i,j),f44(i,j),f45(i,j),f46(i,j),f47(i,j),f48(i,j),f49(i,j),f50(i,j),&
     f51(i,j),f52(i,j),f53(i,j),f54(i,j),f55(i,j),f56(i,j),f57(i,j),f58(i,j),f59(i,j),f60(i,j),&
     f61(i,j),f62(i,j),f63(i,j),f64(i,j),f65(i,j),f66(i,j),f67(i,j),f68(i,j),f69(i,j),f70(i,j),&
     f71(i,j),f72(i,j),f73(i,j),f74(i,j),f75(i,j),f76(i,j),f77(i,j),f78(i,j),f79(i,j),f80(i,j),&
     f81(i,j),f82(i,j),f83(i,j),f84(i,j),f85(i,j),f86(i,j),f87(i,j),f88(i,j),f89(i,j),f90(i,j),&
     f91(i,j),f92(i,j),f93(i,j),f94(i,j),f95(i,j),f96(i,j),f97(i,j),f98(i,j),f99(i,j),f100(i,j),&
     f101(i,j),f102(i,j),f103(i,j),f104(i,j),f105(i,j),f106(i,j),f107(i,j),f108(i,j),f109(i,j),f110(i,j),&
     f111(i,j),f112(i,j),f113(i,j),f114(i,j),f115(i,j),f116(i,j),f117(i,j),f118(i,j),f119(i,j),&
     !
     ff1(i,j),ff2(i,j),ff3(i,j),ff4(i,j),ff5(i,j),ff6(i,j),ff7(i,j),ff8(i,j),ff9(i,j),ff10(i,j),&
     ff11(i,j),ff12(i,j),ff13(i,j),ff14(i,j),ff15(i,j),ff16(i,j),ff17(i,j),ff18(i,j),ff19(i,j),ff20(i,j),&
     ff21(i,j),ff22(i,j),ff23(i,j),ff24(i,j),ff25(i,j),ff26(i,j),ff27(i,j),ff28(i,j),ff29(i,j),ff30(i,j),&
     ff31(i,j),ff32(i,j),ff33(i,j),ff34(i,j),ff35(i,j),&
     !
     G1(i,j),G2(i,j),G3(i,j),G4(i,j),G5(i,j),G6(i,j),G7(i,j),G8(i,j),dxG1(i,j),dyG1(i,j),dxG2(i,j),dyG2(i,j),&
     a(i,j),d(i,j),kappa(i,j),Kstar(i,j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),dxNh(i,j),dyNh(i,j),dxKK(i,j),dyKK(i,j),&
     dxkappa(i,j),dykappa(i,j),dxk1(i,j),dyk1(i,j),dxk2(i,j),dyk2(i,j),&
     Ak1(i,j),Ak2(i,j),ANh(i,j),AKK(i,j),AdxNh(i,j),AdyNh(i,j),AdxKK(i,j),AdyKK(i,j),Adxk1(i,j),Adyk1(i,j),Adxk2(i,j),Adyk2(i,j),&
     AdxxNh(i,j),AdyyNh(i,j),AdxyNh(i,j))
    !
    kNh(i,j)=rhsNh(H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
     H1y(i,j),H1yy(i,j),H1yz(i,j),H1z(i,j),H1zz(i,j),&
     H2(i,j),H2t(i,j),H2tt(i,j),H2tx(i,j),H2ty(i,j),H2tz(i,j),H2x(i,j),H2xx(i,j),H2xy(i,j),H2xz(i,j),&
     H2y(i,j),H2yy(i,j),H2yz(i,j),H2z(i,j),H2zz(i,j),H1H2(i,j),&
     !
     l11(i,j),l11t(i,j),l11tt(i,j),l11tx(i,j),l11ty(i,j),l11tz(i,j),l11x(i,j),l11xx(i,j),l11xy(i,j),l11xz(i,j),&
     l11y(i,j),l11yy(i,j),l11yz(i,j),l11z(i,j),l11zz(i,j),&
     l12(i,j),l12t(i,j),l12tt(i,j),l12tx(i,j),l12ty(i,j),l12tz(i,j),l12x(i,j),l12xx(i,j),l12xy(i,j),l12xz(i,j),&
     l12y(i,j),l12yy(i,j),l12yz(i,j),l12z(i,j),l12zz(i,j),&
     l13(i,j),l13t(i,j),l13tt(i,j),l13tx(i,j),l13ty(i,j),l13tz(i,j),l13x(i,j),l13xx(i,j),l13xy(i,j),l13xz(i,j),&
     l13y(i,j),l13yy(i,j),l13yz(i,j),l13z(i,j),l13zz(i,j),&
     l21(i,j),l21t(i,j),l21tt(i,j),l21tx(i,j),l21ty(i,j),l21tz(i,j),l21x(i,j),l21xx(i,j),l21xy(i,j),l21xz(i,j),&
     l21y(i,j),l21yy(i,j),l21yz(i,j),l21z(i,j),l21zz(i,j),&
     l22(i,j),l22t(i,j),l22tt(i,j),l22tx(i,j),l22ty(i,j),l22tz(i,j),l22x(i,j),l22xx(i,j),l22xy(i,j),l22xz(i,j),&
     l22y(i,j),l22yy(i,j),l22yz(i,j),l22z(i,j),l22zz(i,j),&
     l23(i,j),l23t(i,j),l23tt(i,j),l23tx(i,j),l23ty(i,j),l23tz(i,j),l23x(i,j),l23xx(i,j),l23xy(i,j),l23xz(i,j),&
     l23y(i,j),l23yy(i,j),l23yz(i,j),l23z(i,j),l23zz(i,j),&
     !
     l11n1(i,j),l21n2(i,j),l11n12(i,j),l21n22(i,j),l112(i,j),l122(i,j),l132(i,j),l212(i,j),l222(i,j),l232(i,j),&
     !
     f1(i,j),f2(i,j),f3(i,j),f4(i,j),f5(i,j),f6(i,j),f7(i,j),f8(i,j),f9(i,j),f10(i,j),&
     f11(i,j),f12(i,j),f13(i,j),f14(i,j),f15(i,j),f16(i,j),f17(i,j),f18(i,j),f19(i,j),f20(i,j),&
     f21(i,j),f22(i,j),f23(i,j),f24(i,j),f25(i,j),f26(i,j),f27(i,j),f28(i,j),f29(i,j),f30(i,j),&
     f31(i,j),f32(i,j),f33(i,j),f34(i,j),f35(i,j),f36(i,j),f37(i,j),f38(i,j),f39(i,j),f40(i,j),&
     f41(i,j),f42(i,j),f43(i,j),f44(i,j),f45(i,j),f46(i,j),f47(i,j),f48(i,j),f49(i,j),f50(i,j),&
     f51(i,j),f52(i,j),f53(i,j),f54(i,j),f55(i,j),f56(i,j),f57(i,j),f58(i,j),f59(i,j),f60(i,j),&
     f61(i,j),f62(i,j),f63(i,j),f64(i,j),f65(i,j),f66(i,j),f67(i,j),f68(i,j),f69(i,j),f70(i,j),&
     f71(i,j),f72(i,j),f73(i,j),f74(i,j),f75(i,j),f76(i,j),f77(i,j),f78(i,j),f79(i,j),f80(i,j),&
     f81(i,j),f82(i,j),f83(i,j),f84(i,j),f85(i,j),f86(i,j),f87(i,j),f88(i,j),f89(i,j),f90(i,j),&
     f91(i,j),f92(i,j),f93(i,j),f94(i,j),f95(i,j),f96(i,j),f97(i,j),f98(i,j),f99(i,j),f100(i,j),&
     f101(i,j),f102(i,j),f103(i,j),f104(i,j),f105(i,j),f106(i,j),f107(i,j),f108(i,j),f109(i,j),f110(i,j),&
     f111(i,j),f112(i,j),f113(i,j),f114(i,j),f115(i,j),f116(i,j),f117(i,j),f118(i,j),f119(i,j),&
     !
     ff1(i,j),ff2(i,j),ff3(i,j),ff4(i,j),ff5(i,j),ff6(i,j),ff7(i,j),ff8(i,j),ff9(i,j),ff10(i,j),&
     ff11(i,j),ff12(i,j),ff13(i,j),ff14(i,j),ff15(i,j),ff16(i,j),ff17(i,j),ff18(i,j),ff19(i,j),ff20(i,j),&
     ff21(i,j),ff22(i,j),ff23(i,j),ff24(i,j),ff25(i,j),ff26(i,j),ff27(i,j),ff28(i,j),ff29(i,j),ff30(i,j),&
     ff31(i,j),ff32(i,j),ff33(i,j),ff34(i,j),ff35(i,j),&
     !
     G1(i,j),G2(i,j),G3(i,j),G4(i,j),G5(i,j),G6(i,j),G7(i,j),G8(i,j),dxG1(i,j),dyG1(i,j),dxG2(i,j),dyG2(i,j),&
     a(i,j),d(i,j),kappa(i,j),Kstar(i,j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),dxNh(i,j),dyNh(i,j),dxKK(i,j),dyKK(i,j),&
     dxkappa(i,j),dykappa(i,j),dxk1(i,j),dyk1(i,j),dxk2(i,j),dyk2(i,j),dxxNh(i,j),dyyNh(i,j),dxyNh(i,j),&
     Ak1(i,j),Ak2(i,j),ANh(i,j),AKK(i,j),AdxNh(i,j),AdyNh(i,j),AdxKK(i,j),AdyKK(i,j),Adxk1(i,j),Adyk1(i,j),Adxk2(i,j),Adyk2(i,j),&
     AdxxNh(i,j),AdyyNh(i,j),AdxyNh(i,j))
 END DO
END DO

END SUBROUTINE k1234

END MODULE module222k1234
