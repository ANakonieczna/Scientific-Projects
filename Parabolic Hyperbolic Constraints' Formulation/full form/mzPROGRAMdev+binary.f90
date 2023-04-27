Program IR_RK4_FD4th6th_dev

USE module001FunctionsVarHl   !fkcje pomocnicze H i l
USE module002FunctionsEv      !analytic (A) k1,k2,Nh,KK
USE module002AnalDerivs       !analytic derivatives of (A) k1,k2,Nh,KK
USE module002FunctionsEvAll
USE module002FunctionsVar     !known: a,d,kappa,dxkappa,dykappa,Kstar,G1-8,dxG1,dyG1,dxG2,dyG2
!
USE module004xyGrid
USE module004Przepisanie
!
USE module111Pochodne
USE module222dzRK4
!
USE module333FunctionsHFind

IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: M1,a1,n1,d1,M2,a2,n2,d2   !masa, spin, przyspieszenie(boost) i przesuniecie(displacement)
!--- wielkosci pomocnicze
REAL(8) :: n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12
!-
REAL(8) :: r1,r2   !promienie czarnych dziur
REAL(8) :: U,zINI,dz,hx,hy   !xMAX=yMAX=zMAX, kroki w kierunkach z, x i y
!
INTEGER :: Nx,Ny   !liczba pktow siatki w kierunkach x i y, parzysta ze wzgledu na 'blow up' w x=y=0
!
REAL(8) :: p2   !DSQRT(2)
!
COMMON /PARAMS/ M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy,Nx,Ny,p2
!*************************
REAL(8) :: zPTS
!*************************

INTEGER :: i,j

REAL(8), ALLOCATABLE, DIMENSION(:) :: x,y   !pkty siatki w kierunkach x i y
REAL(8) :: z,zpz   !pkty siatki w kierunku z
INTEGER :: LICZz   !licznik z

REAL(8), ALLOCATABLE, DIMENSION(:,:) :: k1,k2,Nh,KK   !dewiacje
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: dxNh,dyNh,dxxNh,dyyNh,dxyNh,dxKK,dyKK,dxk1,dyk1,dxk2,dyk2   !pochodne dewiacji
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: Ak1,Ak2,ANh,AKK   !tlo
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: AdxNh,AdyNh,AdxxNh,AdyyNh,AdxyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2   !pochodne tla
!--- fkcje pomocnicze
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H1,H1t,H1tt,H1tx,H1ty,H1tz,H1x,H1xx,H1xy,H1xz,H1y,H1yy,H1yz,H1z,H1zz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H2,H2t,H2tt,H2tx,H2ty,H2tz,H2x,H2xx,H2xy,H2xz,H2y,H2yy,H2yz,H2z,H2zz,H1H2
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l11,l11t,l11tt,l11tx,l11ty,l11tz,l11x,l11xx,l11xy,l11xz,l11y,l11yy,l11yz,l11z,l11zz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l12,l12t,l12tt,l12tx,l12ty,l12tz,l12x,l12xx,l12xy,l12xz,l12y,l12yy,l12yz,l12z,l12zz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l13,l13t,l13tt,l13tx,l13ty,l13tz,l13x,l13xx,l13xy,l13xz,l13y,l13yy,l13yz,l13z,l13zz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l21,l21t,l21tt,l21tx,l21ty,l21tz,l21x,l21xx,l21xy,l21xz,l21y,l21yy,l21yz,l21z,l21zz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l22,l22t,l22tt,l22tx,l22ty,l22tz,l22x,l22xx,l22xy,l22xz,l22y,l22yy,l22yz,l22z,l22zz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l23,l23t,l23tt,l23tx,l23ty,l23tz,l23x,l23xx,l23xy,l23xz,l23y,l23yy,l23yz,l23z,l23zz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l11n1,l21n2,l11n12,l21n22,l112,l122,l132,l212,l222,l232
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36,f37,f38,f39,f40
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f41,f42,f43,f44,f45,f46,f47,f48,f49,f50,f51,f52,f53,f54,f55,f56,f57,f58,f59,f60
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f61,f62,f63,f64,f65,f66,f67,f68,f69,f70,f71,f72,f73,f74,f75,f76,f77,f78,f79,f80
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f81,f82,f83,f84,f85,f86,f87,f88,f89,f90,f91,f92,f93,f94,f95,f96,f97,f98,f99,f100
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f101,f102,f103,f104,f105,f106,f107,f108,f109,f110,f111,f112,f113,f114,f115
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f116,f117,f118,f119
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ff1,ff2,ff3,ff4,ff5,ff6,ff7,ff8,ff9,ff10,ff11,ff12,ff13,ff14,ff15,ff16,ff17,ff18,ff19,ff20
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ff21,ff22,ff23,ff24,ff25,ff26,ff27,ff28,ff29,ff30,ff31,ff32,ff33,ff34,ff35
!
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H1pz,H1tpz,H1ttpz,H1txpz,H1typz,H1tzpz,H1xpz,H1xxpz,H1xypz,H1xzpz,H1ypz,H1yypz,H1yzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H1zpz,H1zzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H2pz,H2tpz,H2ttpz,H2txpz,H2typz,H2tzpz,H2xpz,H2xxpz,H2xypz,H2xzpz,H2ypz,H2yypz,H2yzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H2zpz,H2zzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H1H2pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l11pz,l11tpz,l11ttpz,l11txpz,l11typz,l11tzpz,l11xpz,l11xxpz,l11xypz,l11xzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l11ypz,l11yypz,l11yzpz,l11zpz,l11zzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l12pz,l12tpz,l12ttpz,l12txpz,l12typz,l12tzpz,l12xpz,l12xxpz,l12xypz,l12xzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l12ypz,l12yypz,l12yzpz,l12zpz,l12zzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l13pz,l13tpz,l13ttpz,l13txpz,l13typz,l13tzpz,l13xpz,l13xxpz,l13xypz,l13xzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l13ypz,l13yypz,l13yzpz,l13zpz,l13zzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l21pz,l21tpz,l21ttpz,l21txpz,l21typz,l21tzpz,l21xpz,l21xxpz,l21xypz,l21xzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l21ypz,l21yypz,l21yzpz,l21zpz,l21zzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l22pz,l22tpz,l22ttpz,l22txpz,l22typz,l22tzpz,l22xpz,l22xxpz,l22xypz,l22xzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l22ypz,l22yypz,l22yzpz,l22zpz,l22zzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l23pz,l23tpz,l23ttpz,l23txpz,l23typz,l23tzpz,l23xpz,l23xxpz,l23xypz,l23xzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l23ypz,l23yypz,l23yzpz,l23zpz,l23zzpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l11n1pz,l21n2pz,l11n12pz,l21n22pz,l112pz,l122pz,l132pz,l212pz,l222pz,l232pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f1pz,f2pz,f3pz,f4pz,f5pz,f6pz,f7pz,f8pz,f9pz,f10pz,f11pz,f12pz,f13pz,f14pz,f15pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f16pz,f17pz,f18pz,f19pz,f20pz,f21pz,f22pz,f23pz,f24pz,f25pz,f26pz,f27pz,f28pz,f29pz,f30pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f31pz,f32pz,f33pz,f34pz,f35pz,f36pz,f37pz,f38pz,f39pz,f40pz,f41pz,f42pz,f43pz,f44pz,f45pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f46pz,f47pz,f48pz,f49pz,f50pz,f51pz,f52pz,f53pz,f54pz,f55pz,f56pz,f57pz,f58pz,f59pz,f60pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f61pz,f62pz,f63pz,f64pz,f65pz,f66pz,f67pz,f68pz,f69pz,f70pz,f71pz,f72pz,f73pz,f74pz,f75pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f76pz,f77pz,f78pz,f79pz,f80pz,f81pz,f82pz,f83pz,f84pz,f85pz,f86pz,f87pz,f88pz,f89pz,f90pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f91pz,f92pz,f93pz,f94pz,f95pz,f96pz,f97pz,f98pz,f99pz,f100pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f101pz,f102pz,f103pz,f104pz,f105pz,f106pz,f107pz,f108pz,f109pz,f110pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f111pz,f112pz,f113pz,f114pz,f115pz,f116pz,f117pz,f118pz,f119pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ff1pz,ff2pz,ff3pz,ff4pz,ff5pz,ff6pz,ff7pz,ff8pz,ff9pz,ff10pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ff11pz,ff12pz,ff13pz,ff14pz,ff15pz,ff16pz,ff17pz,ff18pz,ff19pz,ff20pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ff21pz,ff22pz,ff23pz,ff24pz,ff25pz,ff26pz,ff27pz,ff28pz,ff29pz,ff30pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ff31pz,ff32pz,ff33pz,ff34pz,ff35pz
!
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,dxkappa,dykappa
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: G1pz,G2pz,G3pz,G4pz,G5pz,G6pz,G7pz,G8pz,dxG1pz,dyG1pz,dxG2pz,dyG2pz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: apz,dpz,kappapz,Kstarpz,dxkappapz,dykappapz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: Ak1pz,Ak2pz,ANhpz,AKKpz
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: AdxNhpz,AdyNhpz,AdxxNhpz,AdyyNhpz,AdxyNhpz,AdxKKpz,AdyKKpz,Adxk1pz,Adyk1pz,Adxk2pz,Adyk2pz
!
!- calkowanie po z - Runge-Kutta 4 rzedu
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: dzRK4KK,dzRK4k1,dzRK4k2,dzRK4Nh
!
!- HFinder
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: Nhupx,Nhupy,Kcxx,Kcyy,Kcxy
REAL(8) :: hxx,hxy,hyy,hxz,hyz,hzz,Kxx,Kxy,Kyy,Kxz,Kyz,Kzz


OPEN(1,file='aPARAMETERS.data')
OPEN(3,file='aPARAMETERSzPTS.data')

OPEN(31,file='z5.dat')      !z,x,y,k1,k2,Nh,KK,Ak1+k1,Ak2+k2,ANh+Nh,AKK+KK
OPEN(32,file='z25.dat')
OPEN(33,file='z2.dat')
OPEN(34,file='z1.dat')
OPEN(35,file='z05.dat')
OPEN(36,file='z025.dat')
OPEN(37,file='z01.dat')
OPEN(38,file='z005.dat')

OPEN(40,file='z3D.dat')

OPEN(70,file='plots.dat')     !z,x,y,NhSQRT(d),KK+kappa

!- HFinder
OPEN(81,file='hxx.dat')
OPEN(82,file='hxy.dat')
OPEN(83,file='hyy.dat')
OPEN(84,file='hxz.dat')
OPEN(85,file='hyz.dat')
OPEN(86,file='hzz.dat')
OPEN(87,file='Kxx.dat')
OPEN(88,file='Kxy.dat')
OPEN(89,file='Kyy.dat')
OPEN(90,file='Kxz.dat')
OPEN(91,file='Kyz.dat')
OPEN(92,file='Kzz.dat')


!******* PARAMETRY **********************************************************************
READ(1,*) M1
READ(1,*) a1
READ(1,*) n1
READ(1,*) d1
READ(1,*) M2
READ(1,*) a2
READ(1,*) n2
READ(1,*) d2
!
READ(1,*) U
READ(1,*) Nx
READ(1,*) dz
!
n12=n1*n1
n22=n2*n2
a12=a1*a1
a22=a2*a2
n1n2=n1*n2
mn1=1.D0-n12
mn2=1.D0-n22
sq1=DSQRT(mn1)
sq2=DSQRT(mn2)
sq12=sq1*sq2
!
r1=M1+DSQRT(M1*M1-a12)
r2=M2+DSQRT(M2*M2-a22)
!
Ny=Nx
hx=2.D0*U/(Nx-1.D0)
hy=2.D0*U/(Ny-1.D0)
zINI=U
!
p2=DSQRT(2.D0)
!****************************************************************************************
READ(3,*) zPTS
!****************************************************************************************

ALLOCATE(x(Nx),y(Ny),&
 k1(Nx,Ny),k2(Nx,Ny),Nh(Nx,Ny),KK(Nx,Ny),&
 dxNh(Nx,Ny),dyNh(Nx,Ny),dxxNh(Nx,Ny),dyyNh(Nx,Ny),dxyNh(Nx,Ny),dxKK(Nx,Ny),dyKK(Nx,Ny),&
 dxk1(Nx,Ny),dyk1(Nx,Ny),dxk2(Nx,Ny),dyk2(Nx,Ny),&
 Ak1(Nx,Ny),Ak2(Nx,Ny),ANh(Nx,Ny),AKK(Nx,Ny),&
 AdxNh(Nx,Ny),AdyNh(Nx,Ny),AdxxNh(Nx,Ny),AdyyNh(Nx,Ny),AdxyNh(Nx,Ny),AdxKK(Nx,Ny),AdyKK(Nx,Ny),&
 Adxk1(Nx,Ny),Adyk1(Nx,Ny),Adxk2(Nx,Ny),Adyk2(Nx,Ny),&
 H1(Nx,Ny),H1t(Nx,Ny),H1tt(Nx,Ny),H1tx(Nx,Ny),H1ty(Nx,Ny),H1tz(Nx,Ny),H1x(Nx,Ny),H1xx(Nx,Ny),H1xy(Nx,Ny),H1xz(Nx,Ny),&
 H1y(Nx,Ny),H1yy(Nx,Ny),H1yz(Nx,Ny),H1z(Nx,Ny),H1zz(Nx,Ny),&
 H2(Nx,Ny),H2t(Nx,Ny),H2tt(Nx,Ny),H2tx(Nx,Ny),H2ty(Nx,Ny),H2tz(Nx,Ny),H2x(Nx,Ny),H2xx(Nx,Ny),H2xy(Nx,Ny),H2xz(Nx,Ny),&
 H2y(Nx,Ny),H2yy(Nx,Ny),H2yz(Nx,Ny),H2z(Nx,Ny),H2zz(Nx,Ny),H1H2(Nx,Ny),&
 l11(Nx,Ny),l11t(Nx,Ny),l11tt(Nx,Ny),l11tx(Nx,Ny),l11ty(Nx,Ny),l11tz(Nx,Ny),&
 l11x(Nx,Ny),l11xx(Nx,Ny),l11xy(Nx,Ny),l11xz(Nx,Ny),l11y(Nx,Ny),l11yy(Nx,Ny),l11yz(Nx,Ny),l11z(Nx,Ny),l11zz(Nx,Ny),&
 l12(Nx,Ny),l12t(Nx,Ny),l12tt(Nx,Ny),l12tx(Nx,Ny),l12ty(Nx,Ny),l12tz(Nx,Ny),&
 l12x(Nx,Ny),l12xx(Nx,Ny),l12xy(Nx,Ny),l12xz(Nx,Ny),l12y(Nx,Ny),l12yy(Nx,Ny),l12yz(Nx,Ny),l12z(Nx,Ny),l12zz(Nx,Ny),&
 l13(Nx,Ny),l13t(Nx,Ny),l13tt(Nx,Ny),l13tx(Nx,Ny),l13ty(Nx,Ny),l13tz(Nx,Ny),&
 l13x(Nx,Ny),l13xx(Nx,Ny),l13xy(Nx,Ny),l13xz(Nx,Ny),l13y(Nx,Ny),l13yy(Nx,Ny),l13yz(Nx,Ny),l13z(Nx,Ny),l13zz(Nx,Ny),&
 l21(Nx,Ny),l21t(Nx,Ny),l21tt(Nx,Ny),l21tx(Nx,Ny),l21ty(Nx,Ny),l21tz(Nx,Ny),&
 l21x(Nx,Ny),l21xx(Nx,Ny),l21xy(Nx,Ny),l21xz(Nx,Ny),l21y(Nx,Ny),l21yy(Nx,Ny),l21yz(Nx,Ny),l21z(Nx,Ny),l21zz(Nx,Ny),&
 l22(Nx,Ny),l22t(Nx,Ny),l22tt(Nx,Ny),l22tx(Nx,Ny),l22ty(Nx,Ny),l22tz(Nx,Ny),&
 l22x(Nx,Ny),l22xx(Nx,Ny),l22xy(Nx,Ny),l22xz(Nx,Ny),l22y(Nx,Ny),l22yy(Nx,Ny),l22yz(Nx,Ny),l22z(Nx,Ny),l22zz(Nx,Ny),&
 l23(Nx,Ny),l23t(Nx,Ny),l23tt(Nx,Ny),l23tx(Nx,Ny),l23ty(Nx,Ny),l23tz(Nx,Ny),&
 l23x(Nx,Ny),l23xx(Nx,Ny),l23xy(Nx,Ny),l23xz(Nx,Ny),l23y(Nx,Ny),l23yy(Nx,Ny),l23yz(Nx,Ny),l23z(Nx,Ny),l23zz(Nx,Ny),&
 l11n1(Nx,Ny),l21n2(Nx,Ny),l11n12(Nx,Ny),l21n22(Nx,Ny),l112(Nx,Ny),l122(Nx,Ny),l132(Nx,Ny),l212(Nx,Ny),l222(Nx,Ny),l232(Nx,Ny),&
 f1(Nx,Ny),f2(Nx,Ny),f3(Nx,Ny),f4(Nx,Ny),f5(Nx,Ny),f6(Nx,Ny),f7(Nx,Ny),f8(Nx,Ny),f9(Nx,Ny),f10(Nx,Ny),&
 f11(Nx,Ny),f12(Nx,Ny),f13(Nx,Ny),f14(Nx,Ny),f15(Nx,Ny),f16(Nx,Ny),f17(Nx,Ny),f18(Nx,Ny),f19(Nx,Ny),f20(Nx,Ny),&
 f21(Nx,Ny),f22(Nx,Ny),f23(Nx,Ny),f24(Nx,Ny),f25(Nx,Ny),f26(Nx,Ny),f27(Nx,Ny),f28(Nx,Ny),f29(Nx,Ny),f30(Nx,Ny),&
 f31(Nx,Ny),f32(Nx,Ny),f33(Nx,Ny),f34(Nx,Ny),f35(Nx,Ny),f36(Nx,Ny),f37(Nx,Ny),f38(Nx,Ny),f39(Nx,Ny),f40(Nx,Ny),&
 f41(Nx,Ny),f42(Nx,Ny),f43(Nx,Ny),f44(Nx,Ny),f45(Nx,Ny),f46(Nx,Ny),f47(Nx,Ny),f48(Nx,Ny),f49(Nx,Ny),f50(Nx,Ny),&
 f51(Nx,Ny),f52(Nx,Ny),f53(Nx,Ny),f54(Nx,Ny),f55(Nx,Ny),f56(Nx,Ny),f57(Nx,Ny),f58(Nx,Ny),f59(Nx,Ny),f60(Nx,Ny),&
 f61(Nx,Ny),f62(Nx,Ny),f63(Nx,Ny),f64(Nx,Ny),f65(Nx,Ny),f66(Nx,Ny),f67(Nx,Ny),f68(Nx,Ny),f69(Nx,Ny),f70(Nx,Ny),&
 f71(Nx,Ny),f72(Nx,Ny),f73(Nx,Ny),f74(Nx,Ny),f75(Nx,Ny),f76(Nx,Ny),f77(Nx,Ny),f78(Nx,Ny),f79(Nx,Ny),f80(Nx,Ny),&
 f81(Nx,Ny),f82(Nx,Ny),f83(Nx,Ny),f84(Nx,Ny),f85(Nx,Ny),f86(Nx,Ny),f87(Nx,Ny),f88(Nx,Ny),f89(Nx,Ny),f90(Nx,Ny),&
 f91(Nx,Ny),f92(Nx,Ny),f93(Nx,Ny),f94(Nx,Ny),f95(Nx,Ny),f96(Nx,Ny),f97(Nx,Ny),f98(Nx,Ny),f99(Nx,Ny),f100(Nx,Ny),&
 f101(Nx,Ny),f102(Nx,Ny),f103(Nx,Ny),f104(Nx,Ny),f105(Nx,Ny),f106(Nx,Ny),f107(Nx,Ny),f108(Nx,Ny),f109(Nx,Ny),f110(Nx,Ny),&
 f111(Nx,Ny),f112(Nx,Ny),f113(Nx,Ny),f114(Nx,Ny),f115(Nx,Ny),f116(Nx,Ny),f117(Nx,Ny),f118(Nx,Ny),f119(Nx,Ny),&
 ff1(Nx,Ny),ff2(Nx,Ny),ff3(Nx,Ny),ff4(Nx,Ny),ff5(Nx,Ny),ff6(Nx,Ny),ff7(Nx,Ny),ff8(Nx,Ny),ff9(Nx,Ny),ff10(Nx,Ny),&
 ff11(Nx,Ny),ff12(Nx,Ny),ff13(Nx,Ny),ff14(Nx,Ny),ff15(Nx,Ny),ff16(Nx,Ny),ff17(Nx,Ny),ff18(Nx,Ny),ff19(Nx,Ny),ff20(Nx,Ny),&
 ff21(Nx,Ny),ff22(Nx,Ny),ff23(Nx,Ny),ff24(Nx,Ny),ff25(Nx,Ny),ff26(Nx,Ny),ff27(Nx,Ny),ff28(Nx,Ny),ff29(Nx,Ny),ff30(Nx,Ny),&
 ff31(Nx,Ny),ff32(Nx,Ny),ff33(Nx,Ny),ff34(Nx,Ny),ff35(Nx,Ny),&
 H1pz(Nx,Ny),H1tpz(Nx,Ny),H1ttpz(Nx,Ny),H1txpz(Nx,Ny),H1typz(Nx,Ny),H1tzpz(Nx,Ny),&
 H1xpz(Nx,Ny),H1xxpz(Nx,Ny),H1xypz(Nx,Ny),H1xzpz(Nx,Ny),H1ypz(Nx,Ny),H1yypz(Nx,Ny),H1yzpz(Nx,Ny),H1zpz(Nx,Ny),H1zzpz(Nx,Ny),&
 H2pz(Nx,Ny),H2tpz(Nx,Ny),H2ttpz(Nx,Ny),H2txpz(Nx,Ny),H2typz(Nx,Ny),H2tzpz(Nx,Ny),&
 H2xpz(Nx,Ny),H2xxpz(Nx,Ny),H2xypz(Nx,Ny),H2xzpz(Nx,Ny),H2ypz(Nx,Ny),H2yypz(Nx,Ny),H2yzpz(Nx,Ny),H2zpz(Nx,Ny),H2zzpz(Nx,Ny),&
 H1H2pz(Nx,Ny),&
 l11pz(Nx,Ny),l11tpz(Nx,Ny),l11ttpz(Nx,Ny),l11txpz(Nx,Ny),l11typz(Nx,Ny),l11tzpz(Nx,Ny),&
 l11xpz(Nx,Ny),l11xxpz(Nx,Ny),l11xypz(Nx,Ny),l11xzpz(Nx,Ny),&
 l11ypz(Nx,Ny),l11yypz(Nx,Ny),l11yzpz(Nx,Ny),l11zpz(Nx,Ny),l11zzpz(Nx,Ny),&
 l12pz(Nx,Ny),l12tpz(Nx,Ny),l12ttpz(Nx,Ny),l12txpz(Nx,Ny),l12typz(Nx,Ny),l12tzpz(Nx,Ny),&
 l12xpz(Nx,Ny),l12xxpz(Nx,Ny),l12xypz(Nx,Ny),l12xzpz(Nx,Ny),&
 l12ypz(Nx,Ny),l12yypz(Nx,Ny),l12yzpz(Nx,Ny),l12zpz(Nx,Ny),l12zzpz(Nx,Ny),&
 l13pz(Nx,Ny),l13tpz(Nx,Ny),l13ttpz(Nx,Ny),l13txpz(Nx,Ny),l13typz(Nx,Ny),l13tzpz(Nx,Ny),&
 l13xpz(Nx,Ny),l13xxpz(Nx,Ny),l13xypz(Nx,Ny),l13xzpz(Nx,Ny),&
 l13ypz(Nx,Ny),l13yypz(Nx,Ny),l13yzpz(Nx,Ny),l13zpz(Nx,Ny),l13zzpz(Nx,Ny),&
 l21pz(Nx,Ny),l21tpz(Nx,Ny),l21ttpz(Nx,Ny),l21txpz(Nx,Ny),l21typz(Nx,Ny),l21tzpz(Nx,Ny),&
 l21xpz(Nx,Ny),l21xxpz(Nx,Ny),l21xypz(Nx,Ny),l21xzpz(Nx,Ny),&
 l21ypz(Nx,Ny),l21yypz(Nx,Ny),l21yzpz(Nx,Ny),l21zpz(Nx,Ny),l21zzpz(Nx,Ny),&
 l22pz(Nx,Ny),l22tpz(Nx,Ny),l22ttpz(Nx,Ny),l22txpz(Nx,Ny),l22typz(Nx,Ny),l22tzpz(Nx,Ny),&
 l22xpz(Nx,Ny),l22xxpz(Nx,Ny),l22xypz(Nx,Ny),l22xzpz(Nx,Ny),&
 l22ypz(Nx,Ny),l22yypz(Nx,Ny),l22yzpz(Nx,Ny),l22zpz(Nx,Ny),l22zzpz(Nx,Ny),&
 l23pz(Nx,Ny),l23tpz(Nx,Ny),l23ttpz(Nx,Ny),l23txpz(Nx,Ny),l23typz(Nx,Ny),l23tzpz(Nx,Ny),&
 l23xpz(Nx,Ny),l23xxpz(Nx,Ny),l23xypz(Nx,Ny),l23xzpz(Nx,Ny),&
 l23ypz(Nx,Ny),l23yypz(Nx,Ny),l23yzpz(Nx,Ny),l23zpz(Nx,Ny),l23zzpz(Nx,Ny),&
 l11n1pz(Nx,Ny),l21n2pz(Nx,Ny),l11n12pz(Nx,Ny),l21n22pz(Nx,Ny),l112pz(Nx,Ny),l122pz(Nx,Ny),l132pz(Nx,Ny),&
 l212pz(Nx,Ny),l222pz(Nx,Ny),l232pz(Nx,Ny),&
 f1pz(Nx,Ny),f2pz(Nx,Ny),f3pz(Nx,Ny),f4pz(Nx,Ny),f5pz(Nx,Ny),f6pz(Nx,Ny),f7pz(Nx,Ny),f8pz(Nx,Ny),f9pz(Nx,Ny),f10pz(Nx,Ny),&
 f11pz(Nx,Ny),f12pz(Nx,Ny),f13pz(Nx,Ny),f14pz(Nx,Ny),f15pz(Nx,Ny),f16pz(Nx,Ny),f17pz(Nx,Ny),f18pz(Nx,Ny),f19pz(Nx,Ny),f20pz(Nx,Ny),&
 f21pz(Nx,Ny),f22pz(Nx,Ny),f23pz(Nx,Ny),f24pz(Nx,Ny),f25pz(Nx,Ny),f26pz(Nx,Ny),f27pz(Nx,Ny),f28pz(Nx,Ny),f29pz(Nx,Ny),f30pz(Nx,Ny),&
 f31pz(Nx,Ny),f32pz(Nx,Ny),f33pz(Nx,Ny),f34pz(Nx,Ny),f35pz(Nx,Ny),f36pz(Nx,Ny),f37pz(Nx,Ny),f38pz(Nx,Ny),f39pz(Nx,Ny),f40pz(Nx,Ny),&
 f41pz(Nx,Ny),f42pz(Nx,Ny),f43pz(Nx,Ny),f44pz(Nx,Ny),f45pz(Nx,Ny),f46pz(Nx,Ny),f47pz(Nx,Ny),f48pz(Nx,Ny),f49pz(Nx,Ny),f50pz(Nx,Ny),&
 f51pz(Nx,Ny),f52pz(Nx,Ny),f53pz(Nx,Ny),f54pz(Nx,Ny),f55pz(Nx,Ny),f56pz(Nx,Ny),f57pz(Nx,Ny),f58pz(Nx,Ny),f59pz(Nx,Ny),f60pz(Nx,Ny),&
 f61pz(Nx,Ny),f62pz(Nx,Ny),f63pz(Nx,Ny),f64pz(Nx,Ny),f65pz(Nx,Ny),f66pz(Nx,Ny),f67pz(Nx,Ny),f68pz(Nx,Ny),f69pz(Nx,Ny),f70pz(Nx,Ny),&
 f71pz(Nx,Ny),f72pz(Nx,Ny),f73pz(Nx,Ny),f74pz(Nx,Ny),f75pz(Nx,Ny),f76pz(Nx,Ny),f77pz(Nx,Ny),f78pz(Nx,Ny),f79pz(Nx,Ny),f80pz(Nx,Ny),&
 f81pz(Nx,Ny),f82pz(Nx,Ny),f83pz(Nx,Ny),f84pz(Nx,Ny),f85pz(Nx,Ny),f86pz(Nx,Ny),f87pz(Nx,Ny),f88pz(Nx,Ny),f89pz(Nx,Ny),f90pz(Nx,Ny),&
 f91pz(Nx,Ny),f92pz(Nx,Ny),f93pz(Nx,Ny),f94pz(Nx,Ny),f95pz(Nx,Ny),&
 f96pz(Nx,Ny),f97pz(Nx,Ny),f98pz(Nx,Ny),f99pz(Nx,Ny),f100pz(Nx,Ny),&
 f101pz(Nx,Ny),f102pz(Nx,Ny),f103pz(Nx,Ny),f104pz(Nx,Ny),f105pz(Nx,Ny),&
 f106pz(Nx,Ny),f107pz(Nx,Ny),f108pz(Nx,Ny),f109pz(Nx,Ny),f110pz(Nx,Ny),&
 f111pz(Nx,Ny),f112pz(Nx,Ny),f113pz(Nx,Ny),f114pz(Nx,Ny),f115pz(Nx,Ny),f116pz(Nx,Ny),f117pz(Nx,Ny),f118pz(Nx,Ny),f119pz(Nx,Ny),&
 ff1pz(Nx,Ny),ff2pz(Nx,Ny),ff3pz(Nx,Ny),ff4pz(Nx,Ny),ff5pz(Nx,Ny),&
 ff6pz(Nx,Ny),ff7pz(Nx,Ny),ff8pz(Nx,Ny),ff9pz(Nx,Ny),ff10pz(Nx,Ny),&
 ff11pz(Nx,Ny),ff12pz(Nx,Ny),ff13pz(Nx,Ny),ff14pz(Nx,Ny),ff15pz(Nx,Ny),&
 ff16pz(Nx,Ny),ff17pz(Nx,Ny),ff18pz(Nx,Ny),ff19pz(Nx,Ny),ff20pz(Nx,Ny),&
 ff21pz(Nx,Ny),ff22pz(Nx,Ny),ff23pz(Nx,Ny),ff24pz(Nx,Ny),ff25pz(Nx,Ny),&
 ff26pz(Nx,Ny),ff27pz(Nx,Ny),ff28pz(Nx,Ny),ff29pz(Nx,Ny),ff30pz(Nx,Ny),&
 ff31pz(Nx,Ny),ff32pz(Nx,Ny),ff33pz(Nx,Ny),ff34pz(Nx,Ny),ff35pz(Nx,Ny),&
 G1(Nx,Ny),G2(Nx,Ny),G3(Nx,Ny),G4(Nx,Ny),G5(Nx,Ny),G6(Nx,Ny),G7(Nx,Ny),G8(Nx,Ny),&
 dxG1(Nx,Ny),dyG1(Nx,Ny),dxG2(Nx,Ny),dyG2(Nx,Ny),a(Nx,Ny),d(Nx,Ny),kappa(Nx,Ny),Kstar(Nx,Ny),dxkappa(Nx,Ny),dykappa(Nx,Ny),&
 G1pz(Nx,Ny),G2pz(Nx,Ny),G3pz(Nx,Ny),G4pz(Nx,Ny),G5pz(Nx,Ny),G6pz(Nx,Ny),G7pz(Nx,Ny),G8pz(Nx,Ny),&
 dxG1pz(Nx,Ny),dyG1pz(Nx,Ny),dxG2pz(Nx,Ny),dyG2pz(Nx,Ny),&
 apz(Nx,Ny),dpz(Nx,Ny),kappapz(Nx,Ny),Kstarpz(Nx,Ny),dxkappapz(Nx,Ny),dykappapz(Nx,Ny),&
 Ak1pz(Nx,Ny),Ak2pz(Nx,Ny),ANhpz(Nx,Ny),AKKpz(Nx,Ny),AdxNhpz(Nx,Ny),AdyNhpz(Nx,Ny),AdxxNhpz(Nx,Ny),AdyyNhpz(Nx,Ny),AdxyNhpz(Nx,Ny),&
 AdxKKpz(Nx,Ny),AdyKKpz(Nx,Ny),Adxk1pz(Nx,Ny),Adyk1pz(Nx,Ny),Adxk2pz(Nx,Ny),Adyk2pz(Nx,Ny),&
 dzRK4KK(Nx,Ny),dzRK4k1(Nx,Ny),dzRK4k2(Nx,Ny),dzRK4Nh(Nx,Ny),&
 Nhupx(Nx,Ny),Nhupy(Nx,Ny),Kcxx(Nx,Ny),Kcyy(Nx,Ny),Kcxy(Nx,Ny))


CALL xyGrid(x,y)

!****************************************************************************************
!******* war pocz na pierwszym z ********************************************************
!****************************************************************************************
z=zINI
LICZz=1

!******* fkcje pomocnicze H i l na siatce (x,y), bez pktow brzegowych *******
DO i=1,Nx
 DO j=1,Ny
    CALL VarFuncHl(x(i),y(j),z,&
     !OUT:
     H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
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
     ff31(i,j),ff32(i,j),ff33(i,j),ff34(i,j),ff35(i,j))
 END DO
END DO

!******* fkcje a,d,kappa,Kstar,G1-8 oraz pochodne kappa,G1,G2 na siatce (x,y), bez pktow brzegowych *******
!******* wartosci fkcji Ak1,Ak2,ANh,AKK oraz ich pochodne na siatce (x,y), bez pktow brzegowych - tlo *******
DO i=1,Nx
 DO j=1,Ny
    CALL VarFunc(H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
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
     !OUT:
     G1(i,j),G2(i,j),G3(i,j),G4(i,j),G5(i,j),G6(i,j),G7(i,j),G8(i,j),dxG1(i,j),dyG1(i,j),dxG2(i,j),dyG2(i,j),&
     a(i,j),d(i,j),kappa(i,j),Kstar(i,j),dxkappa(i,j),dykappa(i,j))
    !
    CALL FunctionsEvAll(H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
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
     !OUT:
     Ak1(i,j),Ak2(i,j),ANh(i,j),AKK(i,j))
    !
    CALL AnalDerivs(H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
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
     !OUT:
     AdxNh(i,j),AdyNh(i,j),AdxKK(i,j),AdyKK(i,j),Adxk1(i,j),Adyk1(i,j),Adxk2(i,j),Adyk2(i,j),AdxxNh(i,j),AdyyNh(i,j),AdxyNh(i,j))
 END DO
END DO

!******* wartosci fkcji k1,k2,Nh,KK na siatce (x,y) - dewiacje *******
k1=0.D0 !k1(Nx,Ny)
k2=0.D0 !k2(Nx,Ny)
Nh=0.D0 !Nh(Nx,Ny)
KK=0.D0 !KK(Nx,Ny)

!******* pochodne fkcji k1,k2,Nh,KK na siatce (x,y) - dewiacje *******
dxNh=0.D0
dyNh=0.D0
dxxNh=0.D0
dyyNh=0.D0
dxyNh=0.D0
dxKK=0.D0
dyKK=0.D0
dxk1=0.D0
dyk1=0.D0
dxk2=0.D0
dyk2=0.D0

!******* przepisanie wartosci na nizszy poziom z *******
CALL Przepisanie(H1,H1t,H1tt,H1tx,H1ty,H1tz,H1x,H1xx,H1xy,H1xz,H1y,H1yy,H1yz,H1z,H1zz,&
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
 G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,dxkappa,dykappa,&
 Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh,z,&
 !OUT:
 H1pz,H1tpz,H1ttpz,H1txpz,H1typz,H1tzpz,H1xpz,H1xxpz,H1xypz,H1xzpz,H1ypz,H1yypz,H1yzpz,H1zpz,H1zzpz,&
 H2pz,H2tpz,H2ttpz,H2txpz,H2typz,H2tzpz,H2xpz,H2xxpz,H2xypz,H2xzpz,H2ypz,H2yypz,H2yzpz,H2zpz,H2zzpz,H1H2pz,&
 !
 l11pz,l11tpz,l11ttpz,l11txpz,l11typz,l11tzpz,l11xpz,l11xxpz,l11xypz,l11xzpz,l11ypz,l11yypz,l11yzpz,l11zpz,l11zzpz,&
 l12pz,l12tpz,l12ttpz,l12txpz,l12typz,l12tzpz,l12xpz,l12xxpz,l12xypz,l12xzpz,l12ypz,l12yypz,l12yzpz,l12zpz,l12zzpz,&
 l13pz,l13tpz,l13ttpz,l13txpz,l13typz,l13tzpz,l13xpz,l13xxpz,l13xypz,l13xzpz,l13ypz,l13yypz,l13yzpz,l13zpz,l13zzpz,&
 l21pz,l21tpz,l21ttpz,l21txpz,l21typz,l21tzpz,l21xpz,l21xxpz,l21xypz,l21xzpz,l21ypz,l21yypz,l21yzpz,l21zpz,l21zzpz,&
 l22pz,l22tpz,l22ttpz,l22txpz,l22typz,l22tzpz,l22xpz,l22xxpz,l22xypz,l22xzpz,l22ypz,l22yypz,l22yzpz,l22zpz,l22zzpz,&
 l23pz,l23tpz,l23ttpz,l23txpz,l23typz,l23tzpz,l23xpz,l23xxpz,l23xypz,l23xzpz,l23ypz,l23yypz,l23yzpz,l23zpz,l23zzpz,&
 !
 l11n1pz,l21n2pz,l11n12pz,l21n22pz,l112pz,l122pz,l132pz,l212pz,l222pz,l232pz,&
 !
 f1pz,f2pz,f3pz,f4pz,f5pz,f6pz,f7pz,f8pz,f9pz,f10pz,f11pz,f12pz,f13pz,f14pz,f15pz,f16pz,f17pz,f18pz,f19pz,f20pz,&
 f21pz,f22pz,f23pz,f24pz,f25pz,f26pz,f27pz,f28pz,f29pz,f30pz,f31pz,f32pz,f33pz,f34pz,f35pz,f36pz,f37pz,f38pz,f39pz,f40pz,&
 f41pz,f42pz,f43pz,f44pz,f45pz,f46pz,f47pz,f48pz,f49pz,f50pz,f51pz,f52pz,f53pz,f54pz,f55pz,f56pz,f57pz,f58pz,f59pz,f60pz,&
 f61pz,f62pz,f63pz,f64pz,f65pz,f66pz,f67pz,f68pz,f69pz,f70pz,f71pz,f72pz,f73pz,f74pz,f75pz,f76pz,f77pz,f78pz,f79pz,f80pz,&
 f81pz,f82pz,f83pz,f84pz,f85pz,f86pz,f87pz,f88pz,f89pz,f90pz,f91pz,f92pz,f93pz,f94pz,f95pz,f96pz,f97pz,f98pz,f99pz,f100pz,&
 f101pz,f102pz,f103pz,f104pz,f105pz,f106pz,f107pz,f108pz,f109pz,f110pz,f111pz,f112pz,f113pz,f114pz,f115pz,&
 f116pz,f117pz,f118pz,f119pz,&
 !
 ff1pz,ff2pz,ff3pz,ff4pz,ff5pz,ff6pz,ff7pz,ff8pz,ff9pz,ff10pz,ff11pz,ff12pz,ff13pz,ff14pz,ff15pz,&
 ff16pz,ff17pz,ff18pz,ff19pz,ff20pz,ff21pz,ff22pz,ff23pz,ff24pz,ff25pz,ff26pz,ff27pz,ff28pz,ff29pz,ff30pz,&
 ff31pz,ff32pz,ff33pz,ff34pz,ff35pz,&
 !
 G1pz,G2pz,G3pz,G4pz,G5pz,G6pz,G7pz,G8pz,dxG1pz,dyG1pz,dxG2pz,dyG2pz,apz,dpz,kappapz,Kstarpz,dxkappapz,dykappapz,&
 Ak1pz,Ak2pz,ANhpz,AKKpz,AdxNhpz,AdyNhpz,AdxKKpz,AdyKKpz,Adxk1pz,Adyk1pz,Adxk2pz,Adyk2pz,AdxxNhpz,AdyyNhpz,AdxyNhpz,zpz)


!****************************************************************************************
!******* ewolucja wzdluz z **************************************************************
DO WHILE (abs(z).GT.abs(dz))
!****************************************************************************************
z=z+dz
LICZz=LICZz+1

!******* fkcje pomocnicze H i l na siatce (x,y), bez pktow brzegowych *******
DO i=1,Nx
 DO j=1,Ny
    CALL VarFuncHl(x(i),y(j),z,&
     !OUT:
     H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
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
     ff31(i,j),ff32(i,j),ff33(i,j),ff34(i,j),ff35(i,j))
 END DO
END DO

!******* fkcje a,d,kappa,Kstar,G1-8 oraz pochodne kappa,G1,G2 na siatce (x,y), bez pktow brzegowych *******
!******* wartosci fkcji Ak1,Ak2,ANh,AKK oraz ich pochodne na siatce (x,y), bez pktow brzegowych - tlo *******
DO i=1,Nx
 DO j=1,Ny
    CALL VarFunc(H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
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
     !OUT:
     G1(i,j),G2(i,j),G3(i,j),G4(i,j),G5(i,j),G6(i,j),G7(i,j),G8(i,j),dxG1(i,j),dyG1(i,j),dxG2(i,j),dyG2(i,j),&
     a(i,j),d(i,j),kappa(i,j),Kstar(i,j),dxkappa(i,j),dykappa(i,j))
    !
    CALL FunctionsEvAll(H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
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
     !OUT:
     Ak1(i,j),Ak2(i,j),ANh(i,j),AKK(i,j))
    !
    CALL AnalDerivs(H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
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
     !OUT:
     AdxNh(i,j),AdyNh(i,j),AdxKK(i,j),AdyKK(i,j),Adxk1(i,j),Adyk1(i,j),Adxk2(i,j),Adyk2(i,j),AdxxNh(i,j),AdyyNh(i,j),AdxyNh(i,j))
 END DO
END DO

!******* fkcje k1,k2,Nh,KK na siatce (x,y), bez pktow brzegowych - dewiacje, RK4 *******
CALL dzRK4(x,y,zpz,&
 H1pz,H1tpz,H1ttpz,H1txpz,H1typz,H1tzpz,H1xpz,H1xxpz,H1xypz,H1xzpz,H1ypz,H1yypz,H1yzpz,H1zpz,H1zzpz,&
 H2pz,H2tpz,H2ttpz,H2txpz,H2typz,H2tzpz,H2xpz,H2xxpz,H2xypz,H2xzpz,H2ypz,H2yypz,H2yzpz,H2zpz,H2zzpz,H1H2pz,&
 !
 l11pz,l11tpz,l11ttpz,l11txpz,l11typz,l11tzpz,l11xpz,l11xxpz,l11xypz,l11xzpz,l11ypz,l11yypz,l11yzpz,l11zpz,l11zzpz,&
 l12pz,l12tpz,l12ttpz,l12txpz,l12typz,l12tzpz,l12xpz,l12xxpz,l12xypz,l12xzpz,l12ypz,l12yypz,l12yzpz,l12zpz,l12zzpz,&
 l13pz,l13tpz,l13ttpz,l13txpz,l13typz,l13tzpz,l13xpz,l13xxpz,l13xypz,l13xzpz,l13ypz,l13yypz,l13yzpz,l13zpz,l13zzpz,&
 l21pz,l21tpz,l21ttpz,l21txpz,l21typz,l21tzpz,l21xpz,l21xxpz,l21xypz,l21xzpz,l21ypz,l21yypz,l21yzpz,l21zpz,l21zzpz,&
 l22pz,l22tpz,l22ttpz,l22txpz,l22typz,l22tzpz,l22xpz,l22xxpz,l22xypz,l22xzpz,l22ypz,l22yypz,l22yzpz,l22zpz,l22zzpz,&
 l23pz,l23tpz,l23ttpz,l23txpz,l23typz,l23tzpz,l23xpz,l23xxpz,l23xypz,l23xzpz,l23ypz,l23yypz,l23yzpz,l23zpz,l23zzpz,&
 !
 l11n1pz,l21n2pz,l11n12pz,l21n22pz,l112pz,l122pz,l132pz,l212pz,l222pz,l232pz,&
 !
 f1pz,f2pz,f3pz,f4pz,f5pz,f6pz,f7pz,f8pz,f9pz,f10pz,f11pz,f12pz,f13pz,f14pz,f15pz,f16pz,f17pz,f18pz,f19pz,f20pz,&
 f21pz,f22pz,f23pz,f24pz,f25pz,f26pz,f27pz,f28pz,f29pz,f30pz,f31pz,f32pz,f33pz,f34pz,f35pz,f36pz,f37pz,f38pz,f39pz,f40pz,&
 f41pz,f42pz,f43pz,f44pz,f45pz,f46pz,f47pz,f48pz,f49pz,f50pz,f51pz,f52pz,f53pz,f54pz,f55pz,f56pz,f57pz,f58pz,f59pz,f60pz,&
 f61pz,f62pz,f63pz,f64pz,f65pz,f66pz,f67pz,f68pz,f69pz,f70pz,f71pz,f72pz,f73pz,f74pz,f75pz,f76pz,f77pz,f78pz,f79pz,f80pz,&
 f81pz,f82pz,f83pz,f84pz,f85pz,f86pz,f87pz,f88pz,f89pz,f90pz,f91pz,f92pz,f93pz,f94pz,f95pz,f96pz,f97pz,f98pz,f99pz,f100pz,&
 f101pz,f102pz,f103pz,f104pz,f105pz,f106pz,f107pz,f108pz,f109pz,f110pz,f111pz,f112pz,f113pz,f114pz,f115pz,&
 f116pz,f117pz,f118pz,f119pz,&
 !
 ff1pz,ff2pz,ff3pz,ff4pz,ff5pz,ff6pz,ff7pz,ff8pz,ff9pz,ff10pz,ff11pz,ff12pz,ff13pz,ff14pz,ff15pz,&
 ff16pz,ff17pz,ff18pz,ff19pz,ff20pz,ff21pz,ff22pz,ff23pz,ff24pz,ff25pz,ff26pz,ff27pz,ff28pz,ff29pz,ff30pz,&
 ff31pz,ff32pz,ff33pz,ff34pz,ff35pz,&
 !
 G1pz,G2pz,G3pz,G4pz,G5pz,G6pz,G7pz,G8pz,dxG1pz,dyG1pz,dxG2pz,dyG2pz,apz,dpz,kappapz,Kstarpz,&
 k1,k2,Nh,KK,dxNh,dyNh,dxKK,dyKK,dxkappapz,dykappapz,dxk1,dyk1,dxk2,dyk2,dxxNh,dyyNh,dxyNh,&
 Ak1pz,Ak2pz,ANhpz,AKKpz,AdxNhpz,AdyNhpz,AdxKKpz,AdyKKpz,Adxk1pz,Adyk1pz,Adxk2pz,Adyk2pz,AdxxNhpz,AdyyNhpz,AdxyNhpz,&
 !
 z,&
 H1,H1t,H1tt,H1tx,H1ty,H1tz,H1x,H1xx,H1xy,H1xz,H1y,H1yy,H1yz,H1z,H1zz,&
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
 G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,dxkappa,dykappa,&
 Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh,&
 !OUT:
 dzRK4KK,dzRK4k1,dzRK4k2,dzRK4Nh)
!
k1=k1+dz*dzRK4k1
k2=k2+dz*dzRK4k2
Nh=Nh+dz*dzRK4Nh
KK=KK+dz*dzRK4KK
!
!******* war brzeg - dewiacje *******
i=1
DO j=1,Ny
   k1(i,j)=0.D0
   k2(i,j)=0.D0
   Nh(i,j)=0.D0
   KK(i,j)=0.D0
END DO
i=Nx
DO j=1,Ny
   k1(i,j)=0.D0
   k2(i,j)=0.D0
   Nh(i,j)=0.D0
   KK(i,j)=0.D0
END DO
!
j=1
DO i=2,Nx-1
   k1(i,j)=0.D0
   k2(i,j)=0.D0
   Nh(i,j)=0.D0
   KK(i,j)=0.D0
END DO
j=Ny
DO i=2,Nx-1
   k1(i,j)=0.D0
   k2(i,j)=0.D0
   Nh(i,j)=0.D0
   KK(i,j)=0.D0
END DO

!******* pochodne fkcji k1,k2,Nh,KK na siatce (x,y), bez pktow brzegowych - dewiacje *******
CALL Pochodne(k1,k2,Nh,KK,&
 !OUT:
 dxNh,dyNh,dxxNh,dyyNh,dxyNh,dxKK,dyKK,dxk1,dyk1,dxk2,dyk2)


!* SPR *! wypisanie fkcji k1,k2,Nh,KK,(A)k1+k1,(A)k2+k2,(A)Nh+Nh,(A)KK+KK do plikow z.dat ------------------------------------
!* 2D
if (LICZz.eq.INT((DABS(U)-5.D0)/DABS(dz))) then   !5
 do i=2,Nx-1
  do j=2,Ny-1
     write(31,11) z,x(i),y(j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(31,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-2.5D0)/DABS(dz))) then   !2.5
 do i=2,Nx-1
  do j=2,Ny-1
     write(32,11) z,x(i),y(j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(32,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-2.D0)/DABS(dz))) then   !2
 do i=2,Nx-1
  do j=2,Ny-1
     write(33,11) z,x(i),y(j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(33,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-1.D0)/DABS(dz))) then   !1
 do i=2,Nx-1
  do j=2,Ny-1
     write(34,11) z,x(i),y(j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(34,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-0.5D0)/DABS(dz))) then   !0.5
 do i=2,Nx-1
  do j=2,Ny-1
     write(35,11) z,x(i),y(j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(35,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-0.25D0)/DABS(dz))) then   !0.25
 do i=2,Nx-1
  do j=2,Ny-1
     write(36,11) z,x(i),y(j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(36,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-0.1D0)/DABS(dz))) then   !0.1
 do i=2,Nx-1
  do j=2,Ny-1
     write(37,11) z,x(i),y(j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(37,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-0.05D0)/DABS(dz))) then   !0.05
 do i=2,Nx-1
  do j=2,Ny-1
     write(38,11) z,x(i),y(j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(38,*)
 end do
end if
!
!* 3D
if (mod(LICZz,INT(DABS(U)/(zPTS*DABS(dz)))).eq.0) then
 do i=1,Nx
  do j=1,Ny
     write(40,11) z,x(i),y(j),k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
     write(70,95) z,x(i),y(j),(ANh(i,j)+Nh(i,j))*DSQRT(d(i,j)),AKK(i,j)+KK(i,j)+kappa(i,j)
     !
     !******* fkcje HFinder *******!
     CALL HFind(H1(i,j),H1t(i,j),H1tt(i,j),H1tx(i,j),H1ty(i,j),H1tz(i,j),H1x(i,j),H1xx(i,j),H1xy(i,j),H1xz(i,j),&
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
      !OUT:
      Nhupx(i,j),Nhupy(i,j),Kcxx(i,j),Kcyy(i,j),Kcxy(i,j))
     !
     hxx=a(i,j)+0.5D0*F8(i,j)
     write(81,94) z,x(i),y(j),hxx
     hxy=0.5D0*F6(i,j)
     write(82,94) z,x(i),y(j),hxy
     hyy=a(i,j)-0.5D0*F8(i,j)
     write(83,94) z,x(i),y(j),hyy
     hxz=hxx*Nhupx(i,j)+hxy*Nhupy(i,j)
     write(84,94) z,x(i),y(j),hxz
     hyz=hyy*Nhupy(i,j)+hxy*Nhupx(i,j)
     write(85,94) z,x(i),y(j),hyz
     hzz=(ANh(i,j)+Nh(i,j))*(ANh(i,j)+Nh(i,j))+hxx*Nhupx(i,j)*Nhupx(i,j)+hyy*Nhupy(i,j)*Nhupy(i,j)+2.D0*hxy*Nhupx(i,j)*Nhupy(i,j)
     write(86,94) z,x(i),y(j),hzz
     Kxx=Kcxx(i,j)+0.5D0*(AKK(i,j)+KK(i,j))*hxx
     write(87,94) z,x(i),y(j),Kxx
     Kxy=Kcxy(i,j)+0.5D0*(AKK(i,j)+KK(i,j))*hxy
     write(88,94) z,x(i),y(j),Kxy
     Kyy=Kcyy(i,j)+0.5D0*(AKK(i,j)+KK(i,j))*hyy
     write(89,94) z,x(i),y(j),Kyy
     Kxz=(ANh(i,j)+Nh(i,j))*(Ak1(i,j)+k1(i,j))+Kxx*Nhupx(i,j)+Kxy*Nhupy(i,j)
     write(90,94) z,x(i),y(j),Kxz
     Kyz=(ANh(i,j)+Nh(i,j))*(Ak2(i,j)+k2(i,j))+Kxy*Nhupx(i,j)+Kyy*Nhupy(i,j)
     write(91,94) z,x(i),y(j),Kyz
     Kzz=kappa(i,j)*(ANh(i,j)+Nh(i,j))*(ANh(i,j)+Nh(i,j))&
      +2.D0*(ANh(i,j)+Nh(i,j))*((Ak1(i,j)+k1(i,j))*Nhupx(i,j)+(Ak2(i,j)+k2(i,j))*Nhupy(i,j))&
      +Kxx*Nhupx(i,j)*Nhupx(i,j)+Kyy*Nhupy(i,j)*Nhupy(i,j)+2.D0*Kxy*Nhupx(i,j)*Nhupy(i,j)
     write(92,94) z,x(i),y(j),Kzz
  end do
!  write(40,*)
!  write(70,*)
 end do
end if
!* *! ----------------------------------------------------------------------------------------------------------------------


!******* przepisanie wartosci na nizszy poziom z *******
CALL Przepisanie(H1,H1t,H1tt,H1tx,H1ty,H1tz,H1x,H1xx,H1xy,H1xz,H1y,H1yy,H1yz,H1z,H1zz,&
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
 G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,dxkappa,dykappa,&
 Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh,z,&
 !OUT:
 H1pz,H1tpz,H1ttpz,H1txpz,H1typz,H1tzpz,H1xpz,H1xxpz,H1xypz,H1xzpz,H1ypz,H1yypz,H1yzpz,H1zpz,H1zzpz,&
 H2pz,H2tpz,H2ttpz,H2txpz,H2typz,H2tzpz,H2xpz,H2xxpz,H2xypz,H2xzpz,H2ypz,H2yypz,H2yzpz,H2zpz,H2zzpz,H1H2pz,&
 !
 l11pz,l11tpz,l11ttpz,l11txpz,l11typz,l11tzpz,l11xpz,l11xxpz,l11xypz,l11xzpz,l11ypz,l11yypz,l11yzpz,l11zpz,l11zzpz,&
 l12pz,l12tpz,l12ttpz,l12txpz,l12typz,l12tzpz,l12xpz,l12xxpz,l12xypz,l12xzpz,l12ypz,l12yypz,l12yzpz,l12zpz,l12zzpz,&
 l13pz,l13tpz,l13ttpz,l13txpz,l13typz,l13tzpz,l13xpz,l13xxpz,l13xypz,l13xzpz,l13ypz,l13yypz,l13yzpz,l13zpz,l13zzpz,&
 l21pz,l21tpz,l21ttpz,l21txpz,l21typz,l21tzpz,l21xpz,l21xxpz,l21xypz,l21xzpz,l21ypz,l21yypz,l21yzpz,l21zpz,l21zzpz,&
 l22pz,l22tpz,l22ttpz,l22txpz,l22typz,l22tzpz,l22xpz,l22xxpz,l22xypz,l22xzpz,l22ypz,l22yypz,l22yzpz,l22zpz,l22zzpz,&
 l23pz,l23tpz,l23ttpz,l23txpz,l23typz,l23tzpz,l23xpz,l23xxpz,l23xypz,l23xzpz,l23ypz,l23yypz,l23yzpz,l23zpz,l23zzpz,&
 !
 l11n1pz,l21n2pz,l11n12pz,l21n22pz,l112pz,l122pz,l132pz,l212pz,l222pz,l232pz,&
 !
 f1pz,f2pz,f3pz,f4pz,f5pz,f6pz,f7pz,f8pz,f9pz,f10pz,f11pz,f12pz,f13pz,f14pz,f15pz,f16pz,f17pz,f18pz,f19pz,f20pz,&
 f21pz,f22pz,f23pz,f24pz,f25pz,f26pz,f27pz,f28pz,f29pz,f30pz,f31pz,f32pz,f33pz,f34pz,f35pz,f36pz,f37pz,f38pz,f39pz,f40pz,&
 f41pz,f42pz,f43pz,f44pz,f45pz,f46pz,f47pz,f48pz,f49pz,f50pz,f51pz,f52pz,f53pz,f54pz,f55pz,f56pz,f57pz,f58pz,f59pz,f60pz,&
 f61pz,f62pz,f63pz,f64pz,f65pz,f66pz,f67pz,f68pz,f69pz,f70pz,f71pz,f72pz,f73pz,f74pz,f75pz,f76pz,f77pz,f78pz,f79pz,f80pz,&
 f81pz,f82pz,f83pz,f84pz,f85pz,f86pz,f87pz,f88pz,f89pz,f90pz,f91pz,f92pz,f93pz,f94pz,f95pz,f96pz,f97pz,f98pz,f99pz,f100pz,&
 f101pz,f102pz,f103pz,f104pz,f105pz,f106pz,f107pz,f108pz,f109pz,f110pz,f111pz,f112pz,f113pz,f114pz,f115pz,&
 f116pz,f117pz,f118pz,f119pz,&
 !
 ff1pz,ff2pz,ff3pz,ff4pz,ff5pz,ff6pz,ff7pz,ff8pz,ff9pz,ff10pz,ff11pz,ff12pz,ff13pz,ff14pz,ff15pz,&
 ff16pz,ff17pz,ff18pz,ff19pz,ff20pz,ff21pz,ff22pz,ff23pz,ff24pz,ff25pz,ff26pz,ff27pz,ff28pz,ff29pz,ff30pz,&
 ff31pz,ff32pz,ff33pz,ff34pz,ff35pz,&
 !
 G1pz,G2pz,G3pz,G4pz,G5pz,G6pz,G7pz,G8pz,dxG1pz,dyG1pz,dxG2pz,dyG2pz,apz,dpz,kappapz,Kstarpz,dxkappapz,dykappapz,&
 Ak1pz,Ak2pz,ANhpz,AKKpz,AdxNhpz,AdyNhpz,AdxKKpz,AdyKKpz,Adxk1pz,Adyk1pz,Adxk2pz,Adyk2pz,AdxxNhpz,AdyyNhpz,AdxyNhpz,zpz)

!****************************************
!******* koniec ewolucji wzdluz z *******
END DO
!****************************************

DEALLOCATE(x,y,k1,k2,Nh,KK,dxNh,dyNh,dxxNh,dyyNh,dxyNh,dxKK,dyKK,dxk1,dyk1,dxk2,dyk2,&
 Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxxNh,AdyyNh,AdxyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,&
 H1,H1t,H1tt,H1tx,H1ty,H1tz,H1x,H1xx,H1xy,H1xz,H1y,H1yy,H1yz,H1z,H1zz,&
 H2,H2t,H2tt,H2tx,H2ty,H2tz,H2x,H2xx,H2xy,H2xz,H2y,H2yy,H2yz,H2z,H2zz,H1H2,&
 l11,l11t,l11tt,l11tx,l11ty,l11tz,l11x,l11xx,l11xy,l11xz,l11y,l11yy,l11yz,l11z,l11zz,&
 l12,l12t,l12tt,l12tx,l12ty,l12tz,l12x,l12xx,l12xy,l12xz,l12y,l12yy,l12yz,l12z,l12zz,&
 l13,l13t,l13tt,l13tx,l13ty,l13tz,l13x,l13xx,l13xy,l13xz,l13y,l13yy,l13yz,l13z,l13zz,&
 l21,l21t,l21tt,l21tx,l21ty,l21tz,l21x,l21xx,l21xy,l21xz,l21y,l21yy,l21yz,l21z,l21zz,&
 l22,l22t,l22tt,l22tx,l22ty,l22tz,l22x,l22xx,l22xy,l22xz,l22y,l22yy,l22yz,l22z,l22zz,&
 l23,l23t,l23tt,l23tx,l23ty,l23tz,l23x,l23xx,l23xy,l23xz,l23y,l23yy,l23yz,l23z,l23zz,&
 l11n1,l21n2,l11n12,l21n22,l112,l122,l132,l212,l222,l232,&
 f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,&
 f31,f32,f33,f34,f35,f36,f37,f38,f39,f40,f41,f42,f43,f44,f45,f46,f47,f48,f49,f50,f51,f52,f53,f54,f55,f56,f57,f58,f59,f60,&
 f61,f62,f63,f64,f65,f66,f67,f68,f69,f70,f71,f72,f73,f74,f75,f76,f77,f78,f79,f80,f81,f82,f83,f84,f85,f86,f87,f88,f89,f90,&
 f91,f92,f93,f94,f95,f96,f97,f98,f99,f100,f101,f102,f103,f104,f105,f106,f107,f108,f109,f110,&
 f111,f112,f113,f114,f115,f116,f117,f118,f119,&
 ff1,ff2,ff3,ff4,ff5,ff6,ff7,ff8,ff9,ff10,ff11,ff12,ff13,ff14,ff15,ff16,ff17,ff18,ff19,ff20,&
 ff21,ff22,ff23,ff24,ff25,ff26,ff27,ff28,ff29,ff30,ff31,ff32,ff33,ff34,ff35,&
 H1pz,H1tpz,H1ttpz,H1txpz,H1typz,H1tzpz,H1xpz,H1xxpz,H1xypz,H1xzpz,H1ypz,H1yypz,H1yzpz,H1zpz,H1zzpz,&
 H2pz,H2tpz,H2ttpz,H2txpz,H2typz,H2tzpz,H2xpz,H2xxpz,H2xypz,H2xzpz,H2ypz,H2yypz,H2yzpz,H2zpz,H2zzpz,H1H2pz,&
 l11pz,l11tpz,l11ttpz,l11txpz,l11typz,l11tzpz,l11xpz,l11xxpz,l11xypz,l11xzpz,l11ypz,l11yypz,l11yzpz,l11zpz,l11zzpz,&
 l12pz,l12tpz,l12ttpz,l12txpz,l12typz,l12tzpz,l12xpz,l12xxpz,l12xypz,l12xzpz,l12ypz,l12yypz,l12yzpz,l12zpz,l12zzpz,&
 l13pz,l13tpz,l13ttpz,l13txpz,l13typz,l13tzpz,l13xpz,l13xxpz,l13xypz,l13xzpz,l13ypz,l13yypz,l13yzpz,l13zpz,l13zzpz,&
 l21pz,l21tpz,l21ttpz,l21txpz,l21typz,l21tzpz,l21xpz,l21xxpz,l21xypz,l21xzpz,l21ypz,l21yypz,l21yzpz,l21zpz,l21zzpz,&
 l22pz,l22tpz,l22ttpz,l22txpz,l22typz,l22tzpz,l22xpz,l22xxpz,l22xypz,l22xzpz,l22ypz,l22yypz,l22yzpz,l22zpz,l22zzpz,&
 l23pz,l23tpz,l23ttpz,l23txpz,l23typz,l23tzpz,l23xpz,l23xxpz,l23xypz,l23xzpz,l23ypz,l23yypz,l23yzpz,l23zpz,l23zzpz,&
 l11n1pz,l21n2pz,l11n12pz,l21n22pz,l112pz,l122pz,l132pz,l212pz,l222pz,l232pz,&
 f1pz,f2pz,f3pz,f4pz,f5pz,f6pz,f7pz,f8pz,f9pz,f10pz,f11pz,f12pz,f13pz,f14pz,f15pz,f16pz,f17pz,f18pz,f19pz,f20pz,&
 f21pz,f22pz,f23pz,f24pz,f25pz,f26pz,f27pz,f28pz,f29pz,f30pz,f31pz,f32pz,f33pz,f34pz,f35pz,f36pz,f37pz,f38pz,f39pz,f40pz,&
 f41pz,f42pz,f43pz,f44pz,f45pz,f46pz,f47pz,f48pz,f49pz,f50pz,f51pz,f52pz,f53pz,f54pz,f55pz,f56pz,f57pz,f58pz,f59pz,f60pz,&
 f61pz,f62pz,f63pz,f64pz,f65pz,f66pz,f67pz,f68pz,f69pz,f70pz,f71pz,f72pz,f73pz,f74pz,f75pz,f76pz,f77pz,f78pz,f79pz,f80pz,&
 f81pz,f82pz,f83pz,f84pz,f85pz,f86pz,f87pz,f88pz,f89pz,f90pz,f91pz,f92pz,f93pz,f94pz,f95pz,f96pz,f97pz,f98pz,f99pz,f100pz,&
 f101pz,f102pz,f103pz,f104pz,f105pz,f106pz,f107pz,f108pz,f109pz,f110pz,f111pz,f112pz,f113pz,f114pz,f115pz,&
 f116pz,f117pz,f118pz,f119pz,&
 ff1pz,ff2pz,ff3pz,ff4pz,ff5pz,ff6pz,ff7pz,ff8pz,ff9pz,ff10pz,ff11pz,ff12pz,ff13pz,ff14pz,ff15pz,&
 ff16pz,ff17pz,ff18pz,ff19pz,ff20pz,ff21pz,ff22pz,ff23pz,ff24pz,ff25pz,ff26pz,ff27pz,ff28pz,ff29pz,ff30pz,&
 ff31pz,ff32pz,ff33pz,ff34pz,ff35pz,&
 G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,dxkappa,dykappa,G1pz,G2pz,G3pz,G4pz,G5pz,G6pz,G7pz,G8pz,&
 dxG1pz,dyG1pz,dxG2pz,dyG2pz,apz,dpz,kappapz,Kstarpz,dxkappapz,dykappapz,&
 Ak1pz,Ak2pz,ANhpz,AKKpz,AdxNhpz,AdyNhpz,AdxxNhpz,AdyyNhpz,AdxyNhpz,AdxKKpz,AdyKKpz,Adxk1pz,Adyk1pz,Adxk2pz,Adyk2pz,&
 dzRK4KK,dzRK4k1,dzRK4k2,dzRK4Nh,&
 Nhupx,Nhupy,Kcxx,Kcyy,Kcxy)


11 FORMAT(11(1X,F20.15))
94 FORMAT(4(1X,F20.15))
95 FORMAT(5(1X,F20.15))

CLOSE(1)
CLOSE(3)

CLOSE(31)
CLOSE(32)
CLOSE(33)
CLOSE(34)
CLOSE(35)
CLOSE(36)
CLOSE(37)

CLOSE(40)

CLOSE(70)

CLOSE(81)
CLOSE(82)
CLOSE(83)
CLOSE(84)
CLOSE(85)
CLOSE(86)
CLOSE(87)
CLOSE(88)
CLOSE(89)
CLOSE(90)
CLOSE(91)
CLOSE(92)

END Program IR_RK4_FD4th6th_dev
