Program IR_RK4_FD4th6th_dev

USE module001FunctionsVarHl   !fkcje pomocnicze H i l
USE module001FunctionsVarHldev
USE module002FunctionsEv      !analytic (A) k1,k2,Nh,KK
USE module002AnalDerivs       !analytic derivatives of (A) k1,k2,Nh,KK
USE module002FunctionsEvAll
USE module002FunctionsEvAlldev
USE module002FunctionsVar     !known: a,d,kappa,dxkappa,dykappa,Kstar,G1-8,dxG1,dyG1,dxG2,dyG2
USE module002FunctionsVardev
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
REAL(8) :: M1dev,a1dev,n1dev,d1dev,M2dev,a2dev,n2dev,d2dev   !masa, spin, przyspieszenie(boost) i przesuniecie(displacement)
!--- wielkosci pomocnicze
REAL(8) :: n12dev,n22dev,a12dev,a22dev,n1n2dev,mn1dev,mn2dev,sq1dev,sq2dev,sq12dev
!-
REAL(8) :: r1dev,r2dev   !promienie czarnych dziur
REAL(8) :: Udev,zINIdev,dzdev,hxdev,hydev   !xMAX=yMAX=zMAX, kroki w kierunkach z, x i y
!
INTEGER :: Nxdev,Nydev   !liczba pktow siatki w kierunkach x i y, parzysta ze wzgledu na 'blow up' w x=y=0
!
REAL(8) :: p2dev   !DSQRT(2)
!
COMMON /PARAMSdev/ M1dev,a1dev,n1dev,d1dev,M2dev,a2dev,n2dev,d2dev,n12dev,n22dev,a12dev,a22dev,&
n1n2dev,mn1dev,mn2dev,sq1dev,sq2dev,sq12dev,r1dev,r2dev,Udev,zINIdev,dzdev,hxdev,hydev,Nxdev,Nydev,p2dev
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
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: k1dev,k2dev,Nhdev,KKdev
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
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H1dev,H1tdev,H1ttdev,H1txdev,H1tydev,H1tzdev,H1xdev,H1xxdev,H1xydev,H1xzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H1ydev,H1yydev,H1yzdev,H1zdev,H1zzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H2dev,H2tdev,H2ttdev,H2txdev,H2tydev,H2tzdev,H2xdev,H2xxdev,H2xydev,H2xzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H2ydev,H2yydev,H2yzdev,H2zdev,H2zzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: H1H2dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l11dev,l11tdev,l11ttdev,l11txdev,l11tydev,l11tzdev,l11xdev,l11xxdev,l11xydev,l11xzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l11ydev,l11yydev,l11yzdev,l11zdev,l11zzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l12dev,l12tdev,l12ttdev,l12txdev,l12tydev,l12tzdev,l12xdev,l12xxdev,l12xydev,l12xzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l12ydev,l12yydev,l12yzdev,l12zdev,l12zzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l13dev,l13tdev,l13ttdev,l13txdev,l13tydev,l13tzdev,l13xdev,l13xxdev,l13xydev,l13xzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l13ydev,l13yydev,l13yzdev,l13zdev,l13zzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l21dev,l21tdev,l21ttdev,l21txdev,l21tydev,l21tzdev,l21xdev,l21xxdev,l21xydev,l21xzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l21ydev,l21yydev,l21yzdev,l21zdev,l21zzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l22dev,l22tdev,l22ttdev,l22txdev,l22tydev,l22tzdev,l22xdev,l22xxdev,l22xydev,l22xzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l22ydev,l22yydev,l22yzdev,l22zdev,l22zzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l23dev,l23tdev,l23ttdev,l23txdev,l23tydev,l23tzdev,l23xdev,l23xxdev,l23xydev,l23xzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l23ydev,l23yydev,l23yzdev,l23zdev,l23zzdev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: l11n1dev,l21n2dev,l11n12dev,l21n22dev,l112dev,l122dev,l132dev,l212dev,l222dev,l232dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f1dev,f2dev,f3dev,f4dev,f5dev,f6dev,f7dev,f8dev,f9dev,f10dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f11dev,f12dev,f13dev,f14dev,f15dev,f16dev,f17dev,f18dev,f19dev,f20dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f21dev,f22dev,f23dev,f24dev,f25dev,f26dev,f27dev,f28dev,f29dev,f30dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f31dev,f32dev,f33dev,f34dev,f35dev,f36dev,f37dev,f38dev,f39dev,f40dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f41dev,f42dev,f43dev,f44dev,f45dev,f46dev,f47dev,f48dev,f49dev,f50dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f51dev,f52dev,f53dev,f54dev,f55dev,f56dev,f57dev,f58dev,f59dev,f60dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f61dev,f62dev,f63dev,f64dev,f65dev,f66dev,f67dev,f68dev,f69dev,f70dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f71dev,f72dev,f73dev,f74dev,f75dev,f76dev,f77dev,f78dev,f79dev,f80dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f81dev,f82dev,f83dev,f84dev,f85dev,f86dev,f87dev,f88dev,f89dev,f90dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f91dev,f92dev,f93dev,f94dev,f95dev,f96dev,f97dev,f98dev,f99dev,f100dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f101dev,f102dev,f103dev,f104dev,f105dev,f106dev,f107dev,f108dev,f109dev,f110dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: f111dev,f112dev,f113dev,f114dev,f115dev,f116dev,f117dev,f118dev,f119dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ff1dev,ff2dev,ff3dev,ff4dev,ff5dev,ff6dev,ff7dev,ff8dev,ff9dev,ff10dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ff11dev,ff12dev,ff13dev,ff14dev,ff15dev,ff16dev,ff17dev,ff18dev,ff19dev,ff20dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ff21dev,ff22dev,ff23dev,ff24dev,ff25dev,ff26dev,ff27dev,ff28dev,ff29dev,ff30dev
REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ff31dev,ff32dev,ff33dev,ff34dev,ff35dev
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

!wypisywanie do pliku
REAL(8) :: L2k1,L2k2,L2Nh,L2KK
REAL(8), ALLOCATABLE, DIMENSION(:) :: Infk1,Infk2,InfNh,InfKK
REAL(8) :: L2xyz1k1,L2xyz1k2,L2xyz1Nh,L2xyz1KK,L2xyz05k1,L2xyz05k2,L2xyz05Nh,L2xyz05KK
REAL(8) :: L2xyz025k1,L2xyz025k2,L2xyz025Nh,L2xyz025KK,L2xyz01k1,L2xyz01k2,L2xyz01Nh,L2xyz01KK
REAL(8) :: max1k1,max1k2,max1Nh,max1KK,max05k1,max05k2,max05Nh,max05KK
REAL(8) :: max025k1,max025k2,max025Nh,max025KK,max01k1,max01k2,max01Nh,max01KK
!
REAL(8) :: G1dev,G2dev,G3dev,G4dev,G5dev,G6dev,G7dev,G8dev,dxG1dev,dyG1dev,dxG2dev,dyG2dev
REAL(8) :: adev,ddev,kappadev,Kstardev,dxkappadev,dykappadev


OPEN(1,file='aPARAMETERS.data')
OPEN(2,file='aPARAMETERSdevBoundary.data')
OPEN(3,file='aPARAMETERSzPTS.data')

OPEN(99,file='norms.dat')   !z,L2k1,L2k2,L2Nh,L2KK,Infk1,Infk2,InfNh,InfKK,bInfk1,bInfk2,bInfNh,bInfKK

OPEN(31,file='z5.dat')      !z,x,y,k1,k2,Nh,KK,Ak1+k1,Ak2+k2,ANh+Nh,AKK+KK
OPEN(32,file='z25.dat')
OPEN(33,file='z2.dat')
OPEN(34,file='z1.dat')
OPEN(35,file='z05.dat')
OPEN(36,file='z025.dat')
OPEN(37,file='z01.dat')
OPEN(38,file='z005.dat')

OPEN(40,file='z3D.dat')

OPEN(62,file='Nxyz.dat')     !z,NORMSxyz1,NORMSxyz05,NORMSxyz025,NORMSxyz01

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
READ(2,*) M1dev
READ(2,*) a1dev
READ(2,*) n1dev
READ(2,*) d1dev
READ(2,*) M2dev
READ(2,*) a2dev
READ(2,*) n2dev
READ(2,*) d2dev
!
READ(2,*) Udev
READ(2,*) Nxdev
READ(2,*) dzdev
!
n12dev=n1dev*n1dev
n22dev=n2dev*n2dev
a12dev=a1dev*a1dev
a22dev=a2dev*a2dev
n1n2dev=n1dev*n2dev
mn1dev=1.D0-n12dev
mn2dev=1.D0-n22dev
sq1dev=DSQRT(mn1dev)
sq2dev=DSQRT(mn2dev)
sq12dev=sq1dev*sq2dev
!
r1dev=M1dev+DSQRT(M1dev*M1dev-a12dev)
r2dev=M2dev+DSQRT(M2dev*M2dev-a22dev)
!
Nydev=Nxdev
hxdev=2.D0*Udev/(Nxdev-1.D0)
hydev=2.D0*Udev/(Nydev-1.D0)
zINIdev=Udev
!
p2dev=DSQRT(2.D0)
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
 k1dev(Nx,Ny),k2dev(Nx,Ny),Nhdev(Nx,Ny),KKdev(Nx,Ny),&
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
 H1dev(Nx,Ny),H1tdev(Nx,Ny),H1ttdev(Nx,Ny),H1txdev(Nx,Ny),H1tydev(Nx,Ny),H1tzdev(Nx,Ny),&
 H1xdev(Nx,Ny),H1xxdev(Nx,Ny),H1xydev(Nx,Ny),H1xzdev(Nx,Ny),H1ydev(Nx,Ny),H1yydev(Nx,Ny),H1yzdev(Nx,Ny),&
 H1zdev(Nx,Ny),H1zzdev(Nx,Ny),&
 H2dev(Nx,Ny),H2tdev(Nx,Ny),H2ttdev(Nx,Ny),H2txdev(Nx,Ny),H2tydev(Nx,Ny),H2tzdev(Nx,Ny),&
 H2xdev(Nx,Ny),H2xxdev(Nx,Ny),H2xydev(Nx,Ny),H2xzdev(Nx,Ny),H2ydev(Nx,Ny),H2yydev(Nx,Ny),H2yzdev(Nx,Ny),&
 H2zdev(Nx,Ny),H2zzdev(Nx,Ny),&
 H1H2dev(Nx,Ny),&
 l11dev(Nx,Ny),l11tdev(Nx,Ny),l11ttdev(Nx,Ny),l11txdev(Nx,Ny),l11tydev(Nx,Ny),l11tzdev(Nx,Ny),&
 l11xdev(Nx,Ny),l11xxdev(Nx,Ny),l11xydev(Nx,Ny),l11xzdev(Nx,Ny),&
 l11ydev(Nx,Ny),l11yydev(Nx,Ny),l11yzdev(Nx,Ny),l11zdev(Nx,Ny),l11zzdev(Nx,Ny),&
 l12dev(Nx,Ny),l12tdev(Nx,Ny),l12ttdev(Nx,Ny),l12txdev(Nx,Ny),l12tydev(Nx,Ny),l12tzdev(Nx,Ny),&
 l12xdev(Nx,Ny),l12xxdev(Nx,Ny),l12xydev(Nx,Ny),l12xzdev(Nx,Ny),&
 l12ydev(Nx,Ny),l12yydev(Nx,Ny),l12yzdev(Nx,Ny),l12zdev(Nx,Ny),l12zzdev(Nx,Ny),&
 l13dev(Nx,Ny),l13tdev(Nx,Ny),l13ttdev(Nx,Ny),l13txdev(Nx,Ny),l13tydev(Nx,Ny),l13tzdev(Nx,Ny),&
 l13xdev(Nx,Ny),l13xxdev(Nx,Ny),l13xydev(Nx,Ny),l13xzdev(Nx,Ny),&
 l13ydev(Nx,Ny),l13yydev(Nx,Ny),l13yzdev(Nx,Ny),l13zdev(Nx,Ny),l13zzdev(Nx,Ny),&
 l21dev(Nx,Ny),l21tdev(Nx,Ny),l21ttdev(Nx,Ny),l21txdev(Nx,Ny),l21tydev(Nx,Ny),l21tzdev(Nx,Ny),&
 l21xdev(Nx,Ny),l21xxdev(Nx,Ny),l21xydev(Nx,Ny),l21xzdev(Nx,Ny),&
 l21ydev(Nx,Ny),l21yydev(Nx,Ny),l21yzdev(Nx,Ny),l21zdev(Nx,Ny),l21zzdev(Nx,Ny),&
 l22dev(Nx,Ny),l22tdev(Nx,Ny),l22ttdev(Nx,Ny),l22txdev(Nx,Ny),l22tydev(Nx,Ny),l22tzdev(Nx,Ny),&
 l22xdev(Nx,Ny),l22xxdev(Nx,Ny),l22xydev(Nx,Ny),l22xzdev(Nx,Ny),&
 l22ydev(Nx,Ny),l22yydev(Nx,Ny),l22yzdev(Nx,Ny),l22zdev(Nx,Ny),l22zzdev(Nx,Ny),&
 l23dev(Nx,Ny),l23tdev(Nx,Ny),l23ttdev(Nx,Ny),l23txdev(Nx,Ny),l23tydev(Nx,Ny),l23tzdev(Nx,Ny),&
 l23xdev(Nx,Ny),l23xxdev(Nx,Ny),l23xydev(Nx,Ny),l23xzdev(Nx,Ny),&
 l23ydev(Nx,Ny),l23yydev(Nx,Ny),l23yzdev(Nx,Ny),l23zdev(Nx,Ny),l23zzdev(Nx,Ny),&
 l11n1dev(Nx,Ny),l21n2dev(Nx,Ny),l11n12dev(Nx,Ny),l21n22dev(Nx,Ny),l112dev(Nx,Ny),l122dev(Nx,Ny),l132dev(Nx,Ny),&
 l212dev(Nx,Ny),l222dev(Nx,Ny),l232dev(Nx,Ny),&
 f1dev(Nx,Ny),f2dev(Nx,Ny),f3dev(Nx,Ny),f4dev(Nx,Ny),f5dev(Nx,Ny),&
 f6dev(Nx,Ny),f7dev(Nx,Ny),f8dev(Nx,Ny),f9dev(Nx,Ny),f10dev(Nx,Ny),&
 f11dev(Nx,Ny),f12dev(Nx,Ny),f13dev(Nx,Ny),f14dev(Nx,Ny),f15dev(Nx,Ny),&
 f16dev(Nx,Ny),f17dev(Nx,Ny),f18dev(Nx,Ny),f19dev(Nx,Ny),f20dev(Nx,Ny),&
 f21dev(Nx,Ny),f22dev(Nx,Ny),f23dev(Nx,Ny),f24dev(Nx,Ny),f25dev(Nx,Ny),&
 f26dev(Nx,Ny),f27dev(Nx,Ny),f28dev(Nx,Ny),f29dev(Nx,Ny),f30dev(Nx,Ny),&
 f31dev(Nx,Ny),f32dev(Nx,Ny),f33dev(Nx,Ny),f34dev(Nx,Ny),f35dev(Nx,Ny),&
 f36dev(Nx,Ny),f37dev(Nx,Ny),f38dev(Nx,Ny),f39dev(Nx,Ny),f40dev(Nx,Ny),&
 f41dev(Nx,Ny),f42dev(Nx,Ny),f43dev(Nx,Ny),f44dev(Nx,Ny),f45dev(Nx,Ny),&
 f46dev(Nx,Ny),f47dev(Nx,Ny),f48dev(Nx,Ny),f49dev(Nx,Ny),f50dev(Nx,Ny),&
 f51dev(Nx,Ny),f52dev(Nx,Ny),f53dev(Nx,Ny),f54dev(Nx,Ny),f55dev(Nx,Ny),&
 f56dev(Nx,Ny),f57dev(Nx,Ny),f58dev(Nx,Ny),f59dev(Nx,Ny),f60dev(Nx,Ny),&
 f61dev(Nx,Ny),f62dev(Nx,Ny),f63dev(Nx,Ny),f64dev(Nx,Ny),f65dev(Nx,Ny),&
 f66dev(Nx,Ny),f67dev(Nx,Ny),f68dev(Nx,Ny),f69dev(Nx,Ny),f70dev(Nx,Ny),&
 f71dev(Nx,Ny),f72dev(Nx,Ny),f73dev(Nx,Ny),f74dev(Nx,Ny),f75dev(Nx,Ny),&
 f76dev(Nx,Ny),f77dev(Nx,Ny),f78dev(Nx,Ny),f79dev(Nx,Ny),f80dev(Nx,Ny),&
 f81dev(Nx,Ny),f82dev(Nx,Ny),f83dev(Nx,Ny),f84dev(Nx,Ny),f85dev(Nx,Ny),&
 f86dev(Nx,Ny),f87dev(Nx,Ny),f88dev(Nx,Ny),f89dev(Nx,Ny),f90dev(Nx,Ny),&
 f91dev(Nx,Ny),f92dev(Nx,Ny),f93dev(Nx,Ny),f94dev(Nx,Ny),f95dev(Nx,Ny),&
 f96dev(Nx,Ny),f97dev(Nx,Ny),f98dev(Nx,Ny),f99dev(Nx,Ny),f100dev(Nx,Ny),&
 f101dev(Nx,Ny),f102dev(Nx,Ny),f103dev(Nx,Ny),f104dev(Nx,Ny),f105dev(Nx,Ny),&
 f106dev(Nx,Ny),f107dev(Nx,Ny),f108dev(Nx,Ny),f109dev(Nx,Ny),f110dev(Nx,Ny),&
 f111dev(Nx,Ny),f112dev(Nx,Ny),f113dev(Nx,Ny),f114dev(Nx,Ny),f115dev(Nx,Ny),&
 f116dev(Nx,Ny),f117dev(Nx,Ny),f118dev(Nx,Ny),f119dev(Nx,Ny),&
 ff1dev(Nx,Ny),ff2dev(Nx,Ny),ff3dev(Nx,Ny),ff4dev(Nx,Ny),ff5dev(Nx,Ny),&
 ff6dev(Nx,Ny),ff7dev(Nx,Ny),ff8dev(Nx,Ny),ff9dev(Nx,Ny),ff10dev(Nx,Ny),&
 ff11dev(Nx,Ny),ff12dev(Nx,Ny),ff13dev(Nx,Ny),ff14dev(Nx,Ny),ff15dev(Nx,Ny),&
 ff16dev(Nx,Ny),ff17dev(Nx,Ny),ff18dev(Nx,Ny),ff19dev(Nx,Ny),ff20dev(Nx,Ny),&
 ff21dev(Nx,Ny),ff22dev(Nx,Ny),ff23dev(Nx,Ny),ff24dev(Nx,Ny),ff25dev(Nx,Ny),&
 ff26dev(Nx,Ny),ff27dev(Nx,Ny),ff28dev(Nx,Ny),ff29dev(Nx,Ny),ff30dev(Nx,Ny),&
 ff31dev(Nx,Ny),ff32dev(Nx,Ny),ff33dev(Nx,Ny),ff34dev(Nx,Ny),ff35dev(Nx,Ny),&
 G1(Nx,Ny),G2(Nx,Ny),G3(Nx,Ny),G4(Nx,Ny),G5(Nx,Ny),G6(Nx,Ny),G7(Nx,Ny),G8(Nx,Ny),&
 dxG1(Nx,Ny),dyG1(Nx,Ny),dxG2(Nx,Ny),dyG2(Nx,Ny),a(Nx,Ny),d(Nx,Ny),kappa(Nx,Ny),Kstar(Nx,Ny),dxkappa(Nx,Ny),dykappa(Nx,Ny),&
 G1pz(Nx,Ny),G2pz(Nx,Ny),G3pz(Nx,Ny),G4pz(Nx,Ny),G5pz(Nx,Ny),G6pz(Nx,Ny),G7pz(Nx,Ny),G8pz(Nx,Ny),&
 dxG1pz(Nx,Ny),dyG1pz(Nx,Ny),dxG2pz(Nx,Ny),dyG2pz(Nx,Ny),&
 apz(Nx,Ny),dpz(Nx,Ny),kappapz(Nx,Ny),Kstarpz(Nx,Ny),dxkappapz(Nx,Ny),dykappapz(Nx,Ny),&
 Ak1pz(Nx,Ny),Ak2pz(Nx,Ny),ANhpz(Nx,Ny),AKKpz(Nx,Ny),AdxNhpz(Nx,Ny),AdyNhpz(Nx,Ny),AdxxNhpz(Nx,Ny),AdyyNhpz(Nx,Ny),AdxyNhpz(Nx,Ny),&
 AdxKKpz(Nx,Ny),AdyKKpz(Nx,Ny),Adxk1pz(Nx,Ny),Adyk1pz(Nx,Ny),Adxk2pz(Nx,Ny),Adyk2pz(Nx,Ny),&
 dzRK4KK(Nx,Ny),dzRK4k1(Nx,Ny),dzRK4k2(Nx,Ny),dzRK4Nh(Nx,Ny),&
 Nhupx(Nx,Ny),Nhupy(Nx,Ny),Kcxx(Nx,Ny),Kcyy(Nx,Ny),Kcxy(Nx,Ny),&
 Infk1(Nx),Infk2(Nx),InfNh(Nx),InfKK(Nx))


CALL xyGrid(x,y)

!****************************************************************************************
!******* war pocz na pierwszym z ********************************************************
!****************************************************************************************
z=zINI
LICZz=1

!******* fkcje pomocnicze H i l na siatce (x,y) *******
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

!******* fkcje a,d,kappa,Kstar,G1-8 oraz pochodne kappa,G1,G2 na siatce (x,y) *******
!******* wartosci fkcji Ak1,Ak2,ANh,AKK oraz ich pochodne na siatce (x,y) - tlo *******
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
DO i=1,Nx
 DO j=1,Ny
    CALL VarFuncHldev(x(i),y(j),z,&
     !OUT:
     H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
     H1xdev(i,j),H1xxdev(i,j),H1xydev(i,j),H1xzdev(i,j),H1ydev(i,j),H1yydev(i,j),H1yzdev(i,j),H1zdev(i,j),H1zzdev(i,j),&
     H2dev(i,j),H2tdev(i,j),H2ttdev(i,j),H2txdev(i,j),H2tydev(i,j),H2tzdev(i,j),&
     H2xdev(i,j),H2xxdev(i,j),H2xydev(i,j),H2xzdev(i,j),H2ydev(i,j),H2yydev(i,j),H2yzdev(i,j),H2zdev(i,j),H2zzdev(i,j),&
     H1H2dev(i,j),&
     !
     l11dev(i,j),l11tdev(i,j),l11ttdev(i,j),l11txdev(i,j),l11tydev(i,j),l11tzdev(i,j),&
     l11xdev(i,j),l11xxdev(i,j),l11xydev(i,j),l11xzdev(i,j),&
     l11ydev(i,j),l11yydev(i,j),l11yzdev(i,j),l11zdev(i,j),l11zzdev(i,j),&
     l12dev(i,j),l12tdev(i,j),l12ttdev(i,j),l12txdev(i,j),l12tydev(i,j),l12tzdev(i,j),&
     l12xdev(i,j),l12xxdev(i,j),l12xydev(i,j),l12xzdev(i,j),&
     l12ydev(i,j),l12yydev(i,j),l12yzdev(i,j),l12zdev(i,j),l12zzdev(i,j),&
     l13dev(i,j),l13tdev(i,j),l13ttdev(i,j),l13txdev(i,j),l13tydev(i,j),l13tzdev(i,j),&
     l13xdev(i,j),l13xxdev(i,j),l13xydev(i,j),l13xzdev(i,j),&
     l13ydev(i,j),l13yydev(i,j),l13yzdev(i,j),l13zdev(i,j),l13zzdev(i,j),&
     l21dev(i,j),l21tdev(i,j),l21ttdev(i,j),l21txdev(i,j),l21tydev(i,j),l21tzdev(i,j),&
     l21xdev(i,j),l21xxdev(i,j),l21xydev(i,j),l21xzdev(i,j),&
     l21ydev(i,j),l21yydev(i,j),l21yzdev(i,j),l21zdev(i,j),l21zzdev(i,j),&
     l22dev(i,j),l22tdev(i,j),l22ttdev(i,j),l22txdev(i,j),l22tydev(i,j),l22tzdev(i,j),&
     l22xdev(i,j),l22xxdev(i,j),l22xydev(i,j),l22xzdev(i,j),&
     l22ydev(i,j),l22yydev(i,j),l22yzdev(i,j),l22zdev(i,j),l22zzdev(i,j),&
     l23dev(i,j),l23tdev(i,j),l23ttdev(i,j),l23txdev(i,j),l23tydev(i,j),l23tzdev(i,j),&
     l23xdev(i,j),l23xxdev(i,j),l23xydev(i,j),l23xzdev(i,j),&
     l23ydev(i,j),l23yydev(i,j),l23yzdev(i,j),l23zdev(i,j),l23zzdev(i,j),&
     !
     l11n1dev(i,j),l21n2dev(i,j),l11n12dev(i,j),l21n22dev(i,j),l112dev(i,j),l122dev(i,j),l132dev(i,j),&
     l212dev(i,j),l222dev(i,j),l232dev(i,j),&
     !
     f1dev(i,j),f2dev(i,j),f3dev(i,j),f4dev(i,j),f5dev(i,j),f6dev(i,j),f7dev(i,j),f8dev(i,j),f9dev(i,j),f10dev(i,j),&
     f11dev(i,j),f12dev(i,j),f13dev(i,j),f14dev(i,j),f15dev(i,j),f16dev(i,j),f17dev(i,j),f18dev(i,j),f19dev(i,j),f20dev(i,j),&
     f21dev(i,j),f22dev(i,j),f23dev(i,j),f24dev(i,j),f25dev(i,j),f26dev(i,j),f27dev(i,j),f28dev(i,j),f29dev(i,j),f30dev(i,j),&
     f31dev(i,j),f32dev(i,j),f33dev(i,j),f34dev(i,j),f35dev(i,j),f36dev(i,j),f37dev(i,j),f38dev(i,j),f39dev(i,j),f40dev(i,j),&
     f41dev(i,j),f42dev(i,j),f43dev(i,j),f44dev(i,j),f45dev(i,j),f46dev(i,j),f47dev(i,j),f48dev(i,j),f49dev(i,j),f50dev(i,j),&
     f51dev(i,j),f52dev(i,j),f53dev(i,j),f54dev(i,j),f55dev(i,j),f56dev(i,j),f57dev(i,j),f58dev(i,j),f59dev(i,j),f60dev(i,j),&
     f61dev(i,j),f62dev(i,j),f63dev(i,j),f64dev(i,j),f65dev(i,j),f66dev(i,j),f67dev(i,j),f68dev(i,j),f69dev(i,j),f70dev(i,j),&
     f71dev(i,j),f72dev(i,j),f73dev(i,j),f74dev(i,j),f75dev(i,j),f76dev(i,j),f77dev(i,j),f78dev(i,j),f79dev(i,j),f80dev(i,j),&
     f81dev(i,j),f82dev(i,j),f83dev(i,j),f84dev(i,j),f85dev(i,j),f86dev(i,j),f87dev(i,j),f88dev(i,j),f89dev(i,j),f90dev(i,j),&
     f91dev(i,j),f92dev(i,j),f93dev(i,j),f94dev(i,j),f95dev(i,j),f96dev(i,j),f97dev(i,j),f98dev(i,j),f99dev(i,j),f100dev(i,j),&
     f101dev(i,j),f102dev(i,j),f103dev(i,j),f104dev(i,j),f105dev(i,j),&
     f106dev(i,j),f107dev(i,j),f108dev(i,j),f109dev(i,j),f110dev(i,j),&
     f111dev(i,j),f112dev(i,j),f113dev(i,j),f114dev(i,j),f115dev(i,j),f116dev(i,j),f117dev(i,j),f118dev(i,j),f119dev(i,j),&
     !
     ff1dev(i,j),ff2dev(i,j),ff3dev(i,j),ff4dev(i,j),ff5dev(i,j),ff6dev(i,j),ff7dev(i,j),ff8dev(i,j),ff9dev(i,j),ff10dev(i,j),&
     ff11dev(i,j),ff12dev(i,j),ff13dev(i,j),ff14dev(i,j),ff15dev(i,j),&
     ff16dev(i,j),ff17dev(i,j),ff18dev(i,j),ff19dev(i,j),ff20dev(i,j),&
     ff21dev(i,j),ff22dev(i,j),ff23dev(i,j),ff24dev(i,j),ff25dev(i,j),&
     ff26dev(i,j),ff27dev(i,j),ff28dev(i,j),ff29dev(i,j),ff30dev(i,j),&
     ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j))
    !
    CALL FunctionsEvAlldev(H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
     H1xdev(i,j),H1xxdev(i,j),H1xydev(i,j),H1xzdev(i,j),H1ydev(i,j),H1yydev(i,j),H1yzdev(i,j),H1zdev(i,j),H1zzdev(i,j),&
     H2dev(i,j),H2tdev(i,j),H2ttdev(i,j),H2txdev(i,j),H2tydev(i,j),H2tzdev(i,j),&
     H2xdev(i,j),H2xxdev(i,j),H2xydev(i,j),H2xzdev(i,j),H2ydev(i,j),H2yydev(i,j),H2yzdev(i,j),H2zdev(i,j),H2zzdev(i,j),&
     H1H2dev(i,j),&
     !
     l11dev(i,j),l11tdev(i,j),l11ttdev(i,j),l11txdev(i,j),l11tydev(i,j),l11tzdev(i,j),&
     l11xdev(i,j),l11xxdev(i,j),l11xydev(i,j),l11xzdev(i,j),&
     l11ydev(i,j),l11yydev(i,j),l11yzdev(i,j),l11zdev(i,j),l11zzdev(i,j),&
     l12dev(i,j),l12tdev(i,j),l12ttdev(i,j),l12txdev(i,j),l12tydev(i,j),l12tzdev(i,j),&
     l12xdev(i,j),l12xxdev(i,j),l12xydev(i,j),l12xzdev(i,j),&
     l12ydev(i,j),l12yydev(i,j),l12yzdev(i,j),l12zdev(i,j),l12zzdev(i,j),&
     l13dev(i,j),l13tdev(i,j),l13ttdev(i,j),l13txdev(i,j),l13tydev(i,j),l13tzdev(i,j),&
     l13xdev(i,j),l13xxdev(i,j),l13xydev(i,j),l13xzdev(i,j),&
     l13ydev(i,j),l13yydev(i,j),l13yzdev(i,j),l13zdev(i,j),l13zzdev(i,j),&
     l21dev(i,j),l21tdev(i,j),l21ttdev(i,j),l21txdev(i,j),l21tydev(i,j),l21tzdev(i,j),&
     l21xdev(i,j),l21xxdev(i,j),l21xydev(i,j),l21xzdev(i,j),&
     l21ydev(i,j),l21yydev(i,j),l21yzdev(i,j),l21zdev(i,j),l21zzdev(i,j),&
     l22dev(i,j),l22tdev(i,j),l22ttdev(i,j),l22txdev(i,j),l22tydev(i,j),l22tzdev(i,j),&
     l22xdev(i,j),l22xxdev(i,j),l22xydev(i,j),l22xzdev(i,j),&
     l22ydev(i,j),l22yydev(i,j),l22yzdev(i,j),l22zdev(i,j),l22zzdev(i,j),&
     l23dev(i,j),l23tdev(i,j),l23ttdev(i,j),l23txdev(i,j),l23tydev(i,j),l23tzdev(i,j),&
     l23xdev(i,j),l23xxdev(i,j),l23xydev(i,j),l23xzdev(i,j),&
     l23ydev(i,j),l23yydev(i,j),l23yzdev(i,j),l23zdev(i,j),l23zzdev(i,j),&
     !
     l11n1dev(i,j),l21n2dev(i,j),l11n12dev(i,j),l21n22dev(i,j),l112dev(i,j),l122dev(i,j),l132dev(i,j),&
     l212dev(i,j),l222dev(i,j),l232dev(i,j),&
     !
     f1dev(i,j),f2dev(i,j),f3dev(i,j),f4dev(i,j),f5dev(i,j),f6dev(i,j),f7dev(i,j),f8dev(i,j),f9dev(i,j),f10dev(i,j),&
     f11dev(i,j),f12dev(i,j),f13dev(i,j),f14dev(i,j),f15dev(i,j),f16dev(i,j),f17dev(i,j),f18dev(i,j),f19dev(i,j),f20dev(i,j),&
     f21dev(i,j),f22dev(i,j),f23dev(i,j),f24dev(i,j),f25dev(i,j),f26dev(i,j),f27dev(i,j),f28dev(i,j),f29dev(i,j),f30dev(i,j),&
     f31dev(i,j),f32dev(i,j),f33dev(i,j),f34dev(i,j),f35dev(i,j),f36dev(i,j),f37dev(i,j),f38dev(i,j),f39dev(i,j),f40dev(i,j),&
     f41dev(i,j),f42dev(i,j),f43dev(i,j),f44dev(i,j),f45dev(i,j),f46dev(i,j),f47dev(i,j),f48dev(i,j),f49dev(i,j),f50dev(i,j),&
     f51dev(i,j),f52dev(i,j),f53dev(i,j),f54dev(i,j),f55dev(i,j),f56dev(i,j),f57dev(i,j),f58dev(i,j),f59dev(i,j),f60dev(i,j),&
     f61dev(i,j),f62dev(i,j),f63dev(i,j),f64dev(i,j),f65dev(i,j),f66dev(i,j),f67dev(i,j),f68dev(i,j),f69dev(i,j),f70dev(i,j),&
     f71dev(i,j),f72dev(i,j),f73dev(i,j),f74dev(i,j),f75dev(i,j),f76dev(i,j),f77dev(i,j),f78dev(i,j),f79dev(i,j),f80dev(i,j),&
     f81dev(i,j),f82dev(i,j),f83dev(i,j),f84dev(i,j),f85dev(i,j),f86dev(i,j),f87dev(i,j),f88dev(i,j),f89dev(i,j),f90dev(i,j),&
     f91dev(i,j),f92dev(i,j),f93dev(i,j),f94dev(i,j),f95dev(i,j),f96dev(i,j),f97dev(i,j),f98dev(i,j),f99dev(i,j),f100dev(i,j),&
     f101dev(i,j),f102dev(i,j),f103dev(i,j),f104dev(i,j),f105dev(i,j),&
     f106dev(i,j),f107dev(i,j),f108dev(i,j),f109dev(i,j),f110dev(i,j),&
     f111dev(i,j),f112dev(i,j),f113dev(i,j),f114dev(i,j),f115dev(i,j),f116dev(i,j),f117dev(i,j),f118dev(i,j),f119dev(i,j),&
     !
     ff1dev(i,j),ff2dev(i,j),ff3dev(i,j),ff4dev(i,j),ff5dev(i,j),ff6dev(i,j),ff7dev(i,j),ff8dev(i,j),ff9dev(i,j),ff10dev(i,j),&
     ff11dev(i,j),ff12dev(i,j),ff13dev(i,j),ff14dev(i,j),ff15dev(i,j),&
     ff16dev(i,j),ff17dev(i,j),ff18dev(i,j),ff19dev(i,j),ff20dev(i,j),&
     ff21dev(i,j),ff22dev(i,j),ff23dev(i,j),ff24dev(i,j),ff25dev(i,j),&
     ff26dev(i,j),ff27dev(i,j),ff28dev(i,j),ff29dev(i,j),ff30dev(i,j),&
     ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j),&
     !OUT:
     k1dev(i,j),k2dev(i,j),Nhdev(i,j),KKdev(i,j))
    ! 
    k1(i,j)=k1dev(i,j)-Ak1(i,j) !k1(Nx,Ny)
    k2(i,j)=k2dev(i,j)-Ak2(i,j) !k2(Nx,Ny)
    Nh(i,j)=Nhdev(i,j)-ANh(i,j) !Nh(Nx,Ny)
    KK(i,j)=KKdev(i,j)-AKK(i,j) !KK(Nx,Ny)
 END DO
END DO

!******* pochodne fkcji k1,k2,Nh,KK na siatce (x,y), bez pktow brzegowych - dewiacje *******
CALL Pochodne(k1,k2,Nh,KK,&
 !OUT:
 dxNh,dyNh,dxxNh,dyyNh,dxyNh,dxKK,dyKK,dxk1,dyk1,dxk2,dyk2)

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


L2xyz1k1=0.D0
L2xyz1k2=0.D0
L2xyz1Nh=0.D0
L2xyz1KK=0.D0
L2xyz05k1=0.D0
L2xyz05k2=0.D0
L2xyz05Nh=0.D0
L2xyz05KK=0.D0
L2xyz025k1=0.D0
L2xyz025k2=0.D0
L2xyz025Nh=0.D0
L2xyz025KK=0.D0
L2xyz01k1=0.D0
L2xyz01k2=0.D0
L2xyz01Nh=0.D0
L2xyz01KK=0.D0
!
max1k1=0.D0
max1k2=0.D0
max1Nh=0.D0
max1KK=0.D0
max05k1=0.D0
max05k2=0.D0
max05Nh=0.D0
max05KK=0.D0
max025k1=0.D0
max025k2=0.D0
max025Nh=0.D0
max025KK=0.D0
max01k1=0.D0
max01k2=0.D0
max01Nh=0.D0
max01KK=0.D0

!****************************************************************************************
!******* ewolucja wzdluz z **************************************************************
DO WHILE (abs(z).GT.abs(dz))
!****************************************************************************************
z=z+dz
LICZz=LICZz+1

!******* fkcje pomocnicze H, l i dev na siatce (x,y) *******
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
    !
    CALL VarFuncHldev(x(i),y(j),z,&
     !OUT:
     H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
     H1xdev(i,j),H1xxdev(i,j),H1xydev(i,j),H1xzdev(i,j),H1ydev(i,j),H1yydev(i,j),H1yzdev(i,j),H1zdev(i,j),H1zzdev(i,j),&
     H2dev(i,j),H2tdev(i,j),H2ttdev(i,j),H2txdev(i,j),H2tydev(i,j),H2tzdev(i,j),&
     H2xdev(i,j),H2xxdev(i,j),H2xydev(i,j),H2xzdev(i,j),H2ydev(i,j),H2yydev(i,j),H2yzdev(i,j),H2zdev(i,j),H2zzdev(i,j),&
     H1H2dev(i,j),&
     !
     l11dev(i,j),l11tdev(i,j),l11ttdev(i,j),l11txdev(i,j),l11tydev(i,j),l11tzdev(i,j),&
     l11xdev(i,j),l11xxdev(i,j),l11xydev(i,j),l11xzdev(i,j),&
     l11ydev(i,j),l11yydev(i,j),l11yzdev(i,j),l11zdev(i,j),l11zzdev(i,j),&
     l12dev(i,j),l12tdev(i,j),l12ttdev(i,j),l12txdev(i,j),l12tydev(i,j),l12tzdev(i,j),&
     l12xdev(i,j),l12xxdev(i,j),l12xydev(i,j),l12xzdev(i,j),&
     l12ydev(i,j),l12yydev(i,j),l12yzdev(i,j),l12zdev(i,j),l12zzdev(i,j),&
     l13dev(i,j),l13tdev(i,j),l13ttdev(i,j),l13txdev(i,j),l13tydev(i,j),l13tzdev(i,j),&
     l13xdev(i,j),l13xxdev(i,j),l13xydev(i,j),l13xzdev(i,j),&
     l13ydev(i,j),l13yydev(i,j),l13yzdev(i,j),l13zdev(i,j),l13zzdev(i,j),&
     l21dev(i,j),l21tdev(i,j),l21ttdev(i,j),l21txdev(i,j),l21tydev(i,j),l21tzdev(i,j),&
     l21xdev(i,j),l21xxdev(i,j),l21xydev(i,j),l21xzdev(i,j),&
     l21ydev(i,j),l21yydev(i,j),l21yzdev(i,j),l21zdev(i,j),l21zzdev(i,j),&
     l22dev(i,j),l22tdev(i,j),l22ttdev(i,j),l22txdev(i,j),l22tydev(i,j),l22tzdev(i,j),&
     l22xdev(i,j),l22xxdev(i,j),l22xydev(i,j),l22xzdev(i,j),&
     l22ydev(i,j),l22yydev(i,j),l22yzdev(i,j),l22zdev(i,j),l22zzdev(i,j),&
     l23dev(i,j),l23tdev(i,j),l23ttdev(i,j),l23txdev(i,j),l23tydev(i,j),l23tzdev(i,j),&
     l23xdev(i,j),l23xxdev(i,j),l23xydev(i,j),l23xzdev(i,j),&
     l23ydev(i,j),l23yydev(i,j),l23yzdev(i,j),l23zdev(i,j),l23zzdev(i,j),&
     !
     l11n1dev(i,j),l21n2dev(i,j),l11n12dev(i,j),l21n22dev(i,j),l112dev(i,j),l122dev(i,j),l132dev(i,j),&
     l212dev(i,j),l222dev(i,j),l232dev(i,j),&
     !
     f1dev(i,j),f2dev(i,j),f3dev(i,j),f4dev(i,j),f5dev(i,j),f6dev(i,j),f7dev(i,j),f8dev(i,j),f9dev(i,j),f10dev(i,j),&
     f11dev(i,j),f12dev(i,j),f13dev(i,j),f14dev(i,j),f15dev(i,j),f16dev(i,j),f17dev(i,j),f18dev(i,j),f19dev(i,j),f20dev(i,j),&
     f21dev(i,j),f22dev(i,j),f23dev(i,j),f24dev(i,j),f25dev(i,j),f26dev(i,j),f27dev(i,j),f28dev(i,j),f29dev(i,j),f30dev(i,j),&
     f31dev(i,j),f32dev(i,j),f33dev(i,j),f34dev(i,j),f35dev(i,j),f36dev(i,j),f37dev(i,j),f38dev(i,j),f39dev(i,j),f40dev(i,j),&
     f41dev(i,j),f42dev(i,j),f43dev(i,j),f44dev(i,j),f45dev(i,j),f46dev(i,j),f47dev(i,j),f48dev(i,j),f49dev(i,j),f50dev(i,j),&
     f51dev(i,j),f52dev(i,j),f53dev(i,j),f54dev(i,j),f55dev(i,j),f56dev(i,j),f57dev(i,j),f58dev(i,j),f59dev(i,j),f60dev(i,j),&
     f61dev(i,j),f62dev(i,j),f63dev(i,j),f64dev(i,j),f65dev(i,j),f66dev(i,j),f67dev(i,j),f68dev(i,j),f69dev(i,j),f70dev(i,j),&
     f71dev(i,j),f72dev(i,j),f73dev(i,j),f74dev(i,j),f75dev(i,j),f76dev(i,j),f77dev(i,j),f78dev(i,j),f79dev(i,j),f80dev(i,j),&
     f81dev(i,j),f82dev(i,j),f83dev(i,j),f84dev(i,j),f85dev(i,j),f86dev(i,j),f87dev(i,j),f88dev(i,j),f89dev(i,j),f90dev(i,j),&
     f91dev(i,j),f92dev(i,j),f93dev(i,j),f94dev(i,j),f95dev(i,j),f96dev(i,j),f97dev(i,j),f98dev(i,j),f99dev(i,j),f100dev(i,j),&
     f101dev(i,j),f102dev(i,j),f103dev(i,j),f104dev(i,j),f105dev(i,j),&
     f106dev(i,j),f107dev(i,j),f108dev(i,j),f109dev(i,j),f110dev(i,j),&
     f111dev(i,j),f112dev(i,j),f113dev(i,j),f114dev(i,j),f115dev(i,j),f116dev(i,j),f117dev(i,j),f118dev(i,j),f119dev(i,j),&
     !
     ff1dev(i,j),ff2dev(i,j),ff3dev(i,j),ff4dev(i,j),ff5dev(i,j),ff6dev(i,j),ff7dev(i,j),ff8dev(i,j),ff9dev(i,j),ff10dev(i,j),&
     ff11dev(i,j),ff12dev(i,j),ff13dev(i,j),ff14dev(i,j),ff15dev(i,j),&
     ff16dev(i,j),ff17dev(i,j),ff18dev(i,j),ff19dev(i,j),ff20dev(i,j),&
     ff21dev(i,j),ff22dev(i,j),ff23dev(i,j),ff24dev(i,j),ff25dev(i,j),&
     ff26dev(i,j),ff27dev(i,j),ff28dev(i,j),ff29dev(i,j),ff30dev(i,j),&
     ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j))
 END DO
END DO

!******* fkcje a,d,kappa,Kstar,G1-8 oraz pochodne kappa,G1,G2 na siatce (x,y) *******
!******* wartosci fkcji Ak1,Ak2,ANh,AKK oraz ich pochodne na siatce (x,y) - tlo *******
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
   CALL FunctionsEvAlldev(H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
    H1xdev(i,j),H1xxdev(i,j),H1xydev(i,j),H1xzdev(i,j),H1ydev(i,j),H1yydev(i,j),H1yzdev(i,j),H1zdev(i,j),H1zzdev(i,j),&
    H2dev(i,j),H2tdev(i,j),H2ttdev(i,j),H2txdev(i,j),H2tydev(i,j),H2tzdev(i,j),&
    H2xdev(i,j),H2xxdev(i,j),H2xydev(i,j),H2xzdev(i,j),H2ydev(i,j),H2yydev(i,j),H2yzdev(i,j),H2zdev(i,j),H2zzdev(i,j),H1H2dev(i,j),&
    !
    l11dev(i,j),l11tdev(i,j),l11ttdev(i,j),l11txdev(i,j),l11tydev(i,j),l11tzdev(i,j),&
    l11xdev(i,j),l11xxdev(i,j),l11xydev(i,j),l11xzdev(i,j),&
    l11ydev(i,j),l11yydev(i,j),l11yzdev(i,j),l11zdev(i,j),l11zzdev(i,j),&
    l12dev(i,j),l12tdev(i,j),l12ttdev(i,j),l12txdev(i,j),l12tydev(i,j),l12tzdev(i,j),&
    l12xdev(i,j),l12xxdev(i,j),l12xydev(i,j),l12xzdev(i,j),&
    l12ydev(i,j),l12yydev(i,j),l12yzdev(i,j),l12zdev(i,j),l12zzdev(i,j),&
    l13dev(i,j),l13tdev(i,j),l13ttdev(i,j),l13txdev(i,j),l13tydev(i,j),l13tzdev(i,j),&
    l13xdev(i,j),l13xxdev(i,j),l13xydev(i,j),l13xzdev(i,j),&
    l13ydev(i,j),l13yydev(i,j),l13yzdev(i,j),l13zdev(i,j),l13zzdev(i,j),&
    l21dev(i,j),l21tdev(i,j),l21ttdev(i,j),l21txdev(i,j),l21tydev(i,j),l21tzdev(i,j),&
    l21xdev(i,j),l21xxdev(i,j),l21xydev(i,j),l21xzdev(i,j),&
    l21ydev(i,j),l21yydev(i,j),l21yzdev(i,j),l21zdev(i,j),l21zzdev(i,j),&
    l22dev(i,j),l22tdev(i,j),l22ttdev(i,j),l22txdev(i,j),l22tydev(i,j),l22tzdev(i,j),&
    l22xdev(i,j),l22xxdev(i,j),l22xydev(i,j),l22xzdev(i,j),&
    l22ydev(i,j),l22yydev(i,j),l22yzdev(i,j),l22zdev(i,j),l22zzdev(i,j),&
    l23dev(i,j),l23tdev(i,j),l23ttdev(i,j),l23txdev(i,j),l23tydev(i,j),l23tzdev(i,j),&
    l23xdev(i,j),l23xxdev(i,j),l23xydev(i,j),l23xzdev(i,j),&
    l23ydev(i,j),l23yydev(i,j),l23yzdev(i,j),l23zdev(i,j),l23zzdev(i,j),&
    !
    l11n1dev(i,j),l21n2dev(i,j),l11n12dev(i,j),l21n22dev(i,j),l112dev(i,j),l122dev(i,j),l132dev(i,j),&
    l212dev(i,j),l222dev(i,j),l232dev(i,j),&
    !
    f1dev(i,j),f2dev(i,j),f3dev(i,j),f4dev(i,j),f5dev(i,j),f6dev(i,j),f7dev(i,j),f8dev(i,j),f9dev(i,j),f10dev(i,j),&
    f11dev(i,j),f12dev(i,j),f13dev(i,j),f14dev(i,j),f15dev(i,j),f16dev(i,j),f17dev(i,j),f18dev(i,j),f19dev(i,j),f20dev(i,j),&
    f21dev(i,j),f22dev(i,j),f23dev(i,j),f24dev(i,j),f25dev(i,j),f26dev(i,j),f27dev(i,j),f28dev(i,j),f29dev(i,j),f30dev(i,j),&
    f31dev(i,j),f32dev(i,j),f33dev(i,j),f34dev(i,j),f35dev(i,j),f36dev(i,j),f37dev(i,j),f38dev(i,j),f39dev(i,j),f40dev(i,j),&
    f41dev(i,j),f42dev(i,j),f43dev(i,j),f44dev(i,j),f45dev(i,j),f46dev(i,j),f47dev(i,j),f48dev(i,j),f49dev(i,j),f50dev(i,j),&
    f51dev(i,j),f52dev(i,j),f53dev(i,j),f54dev(i,j),f55dev(i,j),f56dev(i,j),f57dev(i,j),f58dev(i,j),f59dev(i,j),f60dev(i,j),&
    f61dev(i,j),f62dev(i,j),f63dev(i,j),f64dev(i,j),f65dev(i,j),f66dev(i,j),f67dev(i,j),f68dev(i,j),f69dev(i,j),f70dev(i,j),&
    f71dev(i,j),f72dev(i,j),f73dev(i,j),f74dev(i,j),f75dev(i,j),f76dev(i,j),f77dev(i,j),f78dev(i,j),f79dev(i,j),f80dev(i,j),&
    f81dev(i,j),f82dev(i,j),f83dev(i,j),f84dev(i,j),f85dev(i,j),f86dev(i,j),f87dev(i,j),f88dev(i,j),f89dev(i,j),f90dev(i,j),&
    f91dev(i,j),f92dev(i,j),f93dev(i,j),f94dev(i,j),f95dev(i,j),f96dev(i,j),f97dev(i,j),f98dev(i,j),f99dev(i,j),f100dev(i,j),&
    f101dev(i,j),f102dev(i,j),f103dev(i,j),f104dev(i,j),f105dev(i,j),&
    f106dev(i,j),f107dev(i,j),f108dev(i,j),f109dev(i,j),f110dev(i,j),&
    f111dev(i,j),f112dev(i,j),f113dev(i,j),f114dev(i,j),f115dev(i,j),f116dev(i,j),f117dev(i,j),f118dev(i,j),f119dev(i,j),&
    !
    ff1dev(i,j),ff2dev(i,j),ff3dev(i,j),ff4dev(i,j),ff5dev(i,j),ff6dev(i,j),ff7dev(i,j),ff8dev(i,j),ff9dev(i,j),ff10dev(i,j),&
    ff11dev(i,j),ff12dev(i,j),ff13dev(i,j),ff14dev(i,j),ff15dev(i,j),&
    ff16dev(i,j),ff17dev(i,j),ff18dev(i,j),ff19dev(i,j),ff20dev(i,j),&
    ff21dev(i,j),ff22dev(i,j),ff23dev(i,j),ff24dev(i,j),ff25dev(i,j),&
    ff26dev(i,j),ff27dev(i,j),ff28dev(i,j),ff29dev(i,j),ff30dev(i,j),&
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j),&
    !OUT:
    k1dev(i,j),k2dev(i,j),Nhdev(i,j),KKdev(i,j))
   ! 
   k1(i,j)=k1dev(i,j)-Ak1(i,j)
   k2(i,j)=k2dev(i,j)-Ak2(i,j)
   Nh(i,j)=Nhdev(i,j)-ANh(i,j)
   KK(i,j)=KKdev(i,j)-AKK(i,j)
END DO
i=Nx
DO j=1,Ny
   CALL FunctionsEvAlldev(H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
    H1xdev(i,j),H1xxdev(i,j),H1xydev(i,j),H1xzdev(i,j),H1ydev(i,j),H1yydev(i,j),H1yzdev(i,j),H1zdev(i,j),H1zzdev(i,j),&
    H2dev(i,j),H2tdev(i,j),H2ttdev(i,j),H2txdev(i,j),H2tydev(i,j),H2tzdev(i,j),&
    H2xdev(i,j),H2xxdev(i,j),H2xydev(i,j),H2xzdev(i,j),H2ydev(i,j),H2yydev(i,j),H2yzdev(i,j),H2zdev(i,j),H2zzdev(i,j),H1H2dev(i,j),&
    !
    l11dev(i,j),l11tdev(i,j),l11ttdev(i,j),l11txdev(i,j),l11tydev(i,j),l11tzdev(i,j),&
    l11xdev(i,j),l11xxdev(i,j),l11xydev(i,j),l11xzdev(i,j),&
    l11ydev(i,j),l11yydev(i,j),l11yzdev(i,j),l11zdev(i,j),l11zzdev(i,j),&
    l12dev(i,j),l12tdev(i,j),l12ttdev(i,j),l12txdev(i,j),l12tydev(i,j),l12tzdev(i,j),&
    l12xdev(i,j),l12xxdev(i,j),l12xydev(i,j),l12xzdev(i,j),&
    l12ydev(i,j),l12yydev(i,j),l12yzdev(i,j),l12zdev(i,j),l12zzdev(i,j),&
    l13dev(i,j),l13tdev(i,j),l13ttdev(i,j),l13txdev(i,j),l13tydev(i,j),l13tzdev(i,j),&
    l13xdev(i,j),l13xxdev(i,j),l13xydev(i,j),l13xzdev(i,j),&
    l13ydev(i,j),l13yydev(i,j),l13yzdev(i,j),l13zdev(i,j),l13zzdev(i,j),&
    l21dev(i,j),l21tdev(i,j),l21ttdev(i,j),l21txdev(i,j),l21tydev(i,j),l21tzdev(i,j),&
    l21xdev(i,j),l21xxdev(i,j),l21xydev(i,j),l21xzdev(i,j),&
    l21ydev(i,j),l21yydev(i,j),l21yzdev(i,j),l21zdev(i,j),l21zzdev(i,j),&
    l22dev(i,j),l22tdev(i,j),l22ttdev(i,j),l22txdev(i,j),l22tydev(i,j),l22tzdev(i,j),&
    l22xdev(i,j),l22xxdev(i,j),l22xydev(i,j),l22xzdev(i,j),&
    l22ydev(i,j),l22yydev(i,j),l22yzdev(i,j),l22zdev(i,j),l22zzdev(i,j),&
    l23dev(i,j),l23tdev(i,j),l23ttdev(i,j),l23txdev(i,j),l23tydev(i,j),l23tzdev(i,j),&
    l23xdev(i,j),l23xxdev(i,j),l23xydev(i,j),l23xzdev(i,j),&
    l23ydev(i,j),l23yydev(i,j),l23yzdev(i,j),l23zdev(i,j),l23zzdev(i,j),&
    !
    l11n1dev(i,j),l21n2dev(i,j),l11n12dev(i,j),l21n22dev(i,j),l112dev(i,j),l122dev(i,j),l132dev(i,j),&
    l212dev(i,j),l222dev(i,j),l232dev(i,j),&
    !
    f1dev(i,j),f2dev(i,j),f3dev(i,j),f4dev(i,j),f5dev(i,j),f6dev(i,j),f7dev(i,j),f8dev(i,j),f9dev(i,j),f10dev(i,j),&
    f11dev(i,j),f12dev(i,j),f13dev(i,j),f14dev(i,j),f15dev(i,j),f16dev(i,j),f17dev(i,j),f18dev(i,j),f19dev(i,j),f20dev(i,j),&
    f21dev(i,j),f22dev(i,j),f23dev(i,j),f24dev(i,j),f25dev(i,j),f26dev(i,j),f27dev(i,j),f28dev(i,j),f29dev(i,j),f30dev(i,j),&
    f31dev(i,j),f32dev(i,j),f33dev(i,j),f34dev(i,j),f35dev(i,j),f36dev(i,j),f37dev(i,j),f38dev(i,j),f39dev(i,j),f40dev(i,j),&
    f41dev(i,j),f42dev(i,j),f43dev(i,j),f44dev(i,j),f45dev(i,j),f46dev(i,j),f47dev(i,j),f48dev(i,j),f49dev(i,j),f50dev(i,j),&
    f51dev(i,j),f52dev(i,j),f53dev(i,j),f54dev(i,j),f55dev(i,j),f56dev(i,j),f57dev(i,j),f58dev(i,j),f59dev(i,j),f60dev(i,j),&
    f61dev(i,j),f62dev(i,j),f63dev(i,j),f64dev(i,j),f65dev(i,j),f66dev(i,j),f67dev(i,j),f68dev(i,j),f69dev(i,j),f70dev(i,j),&
    f71dev(i,j),f72dev(i,j),f73dev(i,j),f74dev(i,j),f75dev(i,j),f76dev(i,j),f77dev(i,j),f78dev(i,j),f79dev(i,j),f80dev(i,j),&
    f81dev(i,j),f82dev(i,j),f83dev(i,j),f84dev(i,j),f85dev(i,j),f86dev(i,j),f87dev(i,j),f88dev(i,j),f89dev(i,j),f90dev(i,j),&
    f91dev(i,j),f92dev(i,j),f93dev(i,j),f94dev(i,j),f95dev(i,j),f96dev(i,j),f97dev(i,j),f98dev(i,j),f99dev(i,j),f100dev(i,j),&
    f101dev(i,j),f102dev(i,j),f103dev(i,j),f104dev(i,j),f105dev(i,j),&
    f106dev(i,j),f107dev(i,j),f108dev(i,j),f109dev(i,j),f110dev(i,j),&
    f111dev(i,j),f112dev(i,j),f113dev(i,j),f114dev(i,j),f115dev(i,j),f116dev(i,j),f117dev(i,j),f118dev(i,j),f119dev(i,j),&
    !
    ff1dev(i,j),ff2dev(i,j),ff3dev(i,j),ff4dev(i,j),ff5dev(i,j),ff6dev(i,j),ff7dev(i,j),ff8dev(i,j),ff9dev(i,j),ff10dev(i,j),&
    ff11dev(i,j),ff12dev(i,j),ff13dev(i,j),ff14dev(i,j),ff15dev(i,j),&
    ff16dev(i,j),ff17dev(i,j),ff18dev(i,j),ff19dev(i,j),ff20dev(i,j),&
    ff21dev(i,j),ff22dev(i,j),ff23dev(i,j),ff24dev(i,j),ff25dev(i,j),&
    ff26dev(i,j),ff27dev(i,j),ff28dev(i,j),ff29dev(i,j),ff30dev(i,j),&
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j),&
    !OUT:
    k1dev(i,j),k2dev(i,j),Nhdev(i,j),KKdev(i,j))
   ! 
   k1(i,j)=k1dev(i,j)-Ak1(i,j)
   k2(i,j)=k2dev(i,j)-Ak2(i,j)
   Nh(i,j)=Nhdev(i,j)-ANh(i,j)
   KK(i,j)=KKdev(i,j)-AKK(i,j)
END DO
!
j=1
DO i=2,Nx-1
   CALL FunctionsEvAlldev(H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
    H1xdev(i,j),H1xxdev(i,j),H1xydev(i,j),H1xzdev(i,j),H1ydev(i,j),H1yydev(i,j),H1yzdev(i,j),H1zdev(i,j),H1zzdev(i,j),&
    H2dev(i,j),H2tdev(i,j),H2ttdev(i,j),H2txdev(i,j),H2tydev(i,j),H2tzdev(i,j),&
    H2xdev(i,j),H2xxdev(i,j),H2xydev(i,j),H2xzdev(i,j),H2ydev(i,j),H2yydev(i,j),H2yzdev(i,j),H2zdev(i,j),H2zzdev(i,j),H1H2dev(i,j),&
    !
    l11dev(i,j),l11tdev(i,j),l11ttdev(i,j),l11txdev(i,j),l11tydev(i,j),l11tzdev(i,j),&
    l11xdev(i,j),l11xxdev(i,j),l11xydev(i,j),l11xzdev(i,j),&
    l11ydev(i,j),l11yydev(i,j),l11yzdev(i,j),l11zdev(i,j),l11zzdev(i,j),&
    l12dev(i,j),l12tdev(i,j),l12ttdev(i,j),l12txdev(i,j),l12tydev(i,j),l12tzdev(i,j),&
    l12xdev(i,j),l12xxdev(i,j),l12xydev(i,j),l12xzdev(i,j),&
    l12ydev(i,j),l12yydev(i,j),l12yzdev(i,j),l12zdev(i,j),l12zzdev(i,j),&
    l13dev(i,j),l13tdev(i,j),l13ttdev(i,j),l13txdev(i,j),l13tydev(i,j),l13tzdev(i,j),&
    l13xdev(i,j),l13xxdev(i,j),l13xydev(i,j),l13xzdev(i,j),&
    l13ydev(i,j),l13yydev(i,j),l13yzdev(i,j),l13zdev(i,j),l13zzdev(i,j),&
    l21dev(i,j),l21tdev(i,j),l21ttdev(i,j),l21txdev(i,j),l21tydev(i,j),l21tzdev(i,j),&
    l21xdev(i,j),l21xxdev(i,j),l21xydev(i,j),l21xzdev(i,j),&
    l21ydev(i,j),l21yydev(i,j),l21yzdev(i,j),l21zdev(i,j),l21zzdev(i,j),&
    l22dev(i,j),l22tdev(i,j),l22ttdev(i,j),l22txdev(i,j),l22tydev(i,j),l22tzdev(i,j),&
    l22xdev(i,j),l22xxdev(i,j),l22xydev(i,j),l22xzdev(i,j),&
    l22ydev(i,j),l22yydev(i,j),l22yzdev(i,j),l22zdev(i,j),l22zzdev(i,j),&
    l23dev(i,j),l23tdev(i,j),l23ttdev(i,j),l23txdev(i,j),l23tydev(i,j),l23tzdev(i,j),&
    l23xdev(i,j),l23xxdev(i,j),l23xydev(i,j),l23xzdev(i,j),&
    l23ydev(i,j),l23yydev(i,j),l23yzdev(i,j),l23zdev(i,j),l23zzdev(i,j),&
    !
    l11n1dev(i,j),l21n2dev(i,j),l11n12dev(i,j),l21n22dev(i,j),l112dev(i,j),l122dev(i,j),l132dev(i,j),&
    l212dev(i,j),l222dev(i,j),l232dev(i,j),&
    !
    f1dev(i,j),f2dev(i,j),f3dev(i,j),f4dev(i,j),f5dev(i,j),f6dev(i,j),f7dev(i,j),f8dev(i,j),f9dev(i,j),f10dev(i,j),&
    f11dev(i,j),f12dev(i,j),f13dev(i,j),f14dev(i,j),f15dev(i,j),f16dev(i,j),f17dev(i,j),f18dev(i,j),f19dev(i,j),f20dev(i,j),&
    f21dev(i,j),f22dev(i,j),f23dev(i,j),f24dev(i,j),f25dev(i,j),f26dev(i,j),f27dev(i,j),f28dev(i,j),f29dev(i,j),f30dev(i,j),&
    f31dev(i,j),f32dev(i,j),f33dev(i,j),f34dev(i,j),f35dev(i,j),f36dev(i,j),f37dev(i,j),f38dev(i,j),f39dev(i,j),f40dev(i,j),&
    f41dev(i,j),f42dev(i,j),f43dev(i,j),f44dev(i,j),f45dev(i,j),f46dev(i,j),f47dev(i,j),f48dev(i,j),f49dev(i,j),f50dev(i,j),&
    f51dev(i,j),f52dev(i,j),f53dev(i,j),f54dev(i,j),f55dev(i,j),f56dev(i,j),f57dev(i,j),f58dev(i,j),f59dev(i,j),f60dev(i,j),&
    f61dev(i,j),f62dev(i,j),f63dev(i,j),f64dev(i,j),f65dev(i,j),f66dev(i,j),f67dev(i,j),f68dev(i,j),f69dev(i,j),f70dev(i,j),&
    f71dev(i,j),f72dev(i,j),f73dev(i,j),f74dev(i,j),f75dev(i,j),f76dev(i,j),f77dev(i,j),f78dev(i,j),f79dev(i,j),f80dev(i,j),&
    f81dev(i,j),f82dev(i,j),f83dev(i,j),f84dev(i,j),f85dev(i,j),f86dev(i,j),f87dev(i,j),f88dev(i,j),f89dev(i,j),f90dev(i,j),&
    f91dev(i,j),f92dev(i,j),f93dev(i,j),f94dev(i,j),f95dev(i,j),f96dev(i,j),f97dev(i,j),f98dev(i,j),f99dev(i,j),f100dev(i,j),&
    f101dev(i,j),f102dev(i,j),f103dev(i,j),f104dev(i,j),f105dev(i,j),&
    f106dev(i,j),f107dev(i,j),f108dev(i,j),f109dev(i,j),f110dev(i,j),&
    f111dev(i,j),f112dev(i,j),f113dev(i,j),f114dev(i,j),f115dev(i,j),f116dev(i,j),f117dev(i,j),f118dev(i,j),f119dev(i,j),&
    !
    ff1dev(i,j),ff2dev(i,j),ff3dev(i,j),ff4dev(i,j),ff5dev(i,j),ff6dev(i,j),ff7dev(i,j),ff8dev(i,j),ff9dev(i,j),ff10dev(i,j),&
    ff11dev(i,j),ff12dev(i,j),ff13dev(i,j),ff14dev(i,j),ff15dev(i,j),&
    ff16dev(i,j),ff17dev(i,j),ff18dev(i,j),ff19dev(i,j),ff20dev(i,j),&
    ff21dev(i,j),ff22dev(i,j),ff23dev(i,j),ff24dev(i,j),ff25dev(i,j),&
    ff26dev(i,j),ff27dev(i,j),ff28dev(i,j),ff29dev(i,j),ff30dev(i,j),&
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j),&
    !OUT:
    k1dev(i,j),k2dev(i,j),Nhdev(i,j),KKdev(i,j))
   ! 
   k1(i,j)=k1dev(i,j)-Ak1(i,j)
   k2(i,j)=k2dev(i,j)-Ak2(i,j)
   Nh(i,j)=Nhdev(i,j)-ANh(i,j)
   KK(i,j)=KKdev(i,j)-AKK(i,j)
END DO
j=Ny
DO i=2,Nx-1
   CALL FunctionsEvAlldev(H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
    H1xdev(i,j),H1xxdev(i,j),H1xydev(i,j),H1xzdev(i,j),H1ydev(i,j),H1yydev(i,j),H1yzdev(i,j),H1zdev(i,j),H1zzdev(i,j),&
    H2dev(i,j),H2tdev(i,j),H2ttdev(i,j),H2txdev(i,j),H2tydev(i,j),H2tzdev(i,j),&
    H2xdev(i,j),H2xxdev(i,j),H2xydev(i,j),H2xzdev(i,j),H2ydev(i,j),H2yydev(i,j),H2yzdev(i,j),H2zdev(i,j),H2zzdev(i,j),H1H2dev(i,j),&
    !
    l11dev(i,j),l11tdev(i,j),l11ttdev(i,j),l11txdev(i,j),l11tydev(i,j),l11tzdev(i,j),&
    l11xdev(i,j),l11xxdev(i,j),l11xydev(i,j),l11xzdev(i,j),&
    l11ydev(i,j),l11yydev(i,j),l11yzdev(i,j),l11zdev(i,j),l11zzdev(i,j),&
    l12dev(i,j),l12tdev(i,j),l12ttdev(i,j),l12txdev(i,j),l12tydev(i,j),l12tzdev(i,j),&
    l12xdev(i,j),l12xxdev(i,j),l12xydev(i,j),l12xzdev(i,j),&
    l12ydev(i,j),l12yydev(i,j),l12yzdev(i,j),l12zdev(i,j),l12zzdev(i,j),&
    l13dev(i,j),l13tdev(i,j),l13ttdev(i,j),l13txdev(i,j),l13tydev(i,j),l13tzdev(i,j),&
    l13xdev(i,j),l13xxdev(i,j),l13xydev(i,j),l13xzdev(i,j),&
    l13ydev(i,j),l13yydev(i,j),l13yzdev(i,j),l13zdev(i,j),l13zzdev(i,j),&
    l21dev(i,j),l21tdev(i,j),l21ttdev(i,j),l21txdev(i,j),l21tydev(i,j),l21tzdev(i,j),&
    l21xdev(i,j),l21xxdev(i,j),l21xydev(i,j),l21xzdev(i,j),&
    l21ydev(i,j),l21yydev(i,j),l21yzdev(i,j),l21zdev(i,j),l21zzdev(i,j),&
    l22dev(i,j),l22tdev(i,j),l22ttdev(i,j),l22txdev(i,j),l22tydev(i,j),l22tzdev(i,j),&
    l22xdev(i,j),l22xxdev(i,j),l22xydev(i,j),l22xzdev(i,j),&
    l22ydev(i,j),l22yydev(i,j),l22yzdev(i,j),l22zdev(i,j),l22zzdev(i,j),&
    l23dev(i,j),l23tdev(i,j),l23ttdev(i,j),l23txdev(i,j),l23tydev(i,j),l23tzdev(i,j),&
    l23xdev(i,j),l23xxdev(i,j),l23xydev(i,j),l23xzdev(i,j),&
    l23ydev(i,j),l23yydev(i,j),l23yzdev(i,j),l23zdev(i,j),l23zzdev(i,j),&
    !
    l11n1dev(i,j),l21n2dev(i,j),l11n12dev(i,j),l21n22dev(i,j),l112dev(i,j),l122dev(i,j),l132dev(i,j),&
    l212dev(i,j),l222dev(i,j),l232dev(i,j),&
    !
    f1dev(i,j),f2dev(i,j),f3dev(i,j),f4dev(i,j),f5dev(i,j),f6dev(i,j),f7dev(i,j),f8dev(i,j),f9dev(i,j),f10dev(i,j),&
    f11dev(i,j),f12dev(i,j),f13dev(i,j),f14dev(i,j),f15dev(i,j),f16dev(i,j),f17dev(i,j),f18dev(i,j),f19dev(i,j),f20dev(i,j),&
    f21dev(i,j),f22dev(i,j),f23dev(i,j),f24dev(i,j),f25dev(i,j),f26dev(i,j),f27dev(i,j),f28dev(i,j),f29dev(i,j),f30dev(i,j),&
    f31dev(i,j),f32dev(i,j),f33dev(i,j),f34dev(i,j),f35dev(i,j),f36dev(i,j),f37dev(i,j),f38dev(i,j),f39dev(i,j),f40dev(i,j),&
    f41dev(i,j),f42dev(i,j),f43dev(i,j),f44dev(i,j),f45dev(i,j),f46dev(i,j),f47dev(i,j),f48dev(i,j),f49dev(i,j),f50dev(i,j),&
    f51dev(i,j),f52dev(i,j),f53dev(i,j),f54dev(i,j),f55dev(i,j),f56dev(i,j),f57dev(i,j),f58dev(i,j),f59dev(i,j),f60dev(i,j),&
    f61dev(i,j),f62dev(i,j),f63dev(i,j),f64dev(i,j),f65dev(i,j),f66dev(i,j),f67dev(i,j),f68dev(i,j),f69dev(i,j),f70dev(i,j),&
    f71dev(i,j),f72dev(i,j),f73dev(i,j),f74dev(i,j),f75dev(i,j),f76dev(i,j),f77dev(i,j),f78dev(i,j),f79dev(i,j),f80dev(i,j),&
    f81dev(i,j),f82dev(i,j),f83dev(i,j),f84dev(i,j),f85dev(i,j),f86dev(i,j),f87dev(i,j),f88dev(i,j),f89dev(i,j),f90dev(i,j),&
    f91dev(i,j),f92dev(i,j),f93dev(i,j),f94dev(i,j),f95dev(i,j),f96dev(i,j),f97dev(i,j),f98dev(i,j),f99dev(i,j),f100dev(i,j),&
    f101dev(i,j),f102dev(i,j),f103dev(i,j),f104dev(i,j),f105dev(i,j),&
    f106dev(i,j),f107dev(i,j),f108dev(i,j),f109dev(i,j),f110dev(i,j),&
    f111dev(i,j),f112dev(i,j),f113dev(i,j),f114dev(i,j),f115dev(i,j),f116dev(i,j),f117dev(i,j),f118dev(i,j),f119dev(i,j),&
    !
    ff1dev(i,j),ff2dev(i,j),ff3dev(i,j),ff4dev(i,j),ff5dev(i,j),ff6dev(i,j),ff7dev(i,j),ff8dev(i,j),ff9dev(i,j),ff10dev(i,j),&
    ff11dev(i,j),ff12dev(i,j),ff13dev(i,j),ff14dev(i,j),ff15dev(i,j),&
    ff16dev(i,j),ff17dev(i,j),ff18dev(i,j),ff19dev(i,j),ff20dev(i,j),&
    ff21dev(i,j),ff22dev(i,j),ff23dev(i,j),ff24dev(i,j),ff25dev(i,j),&
    ff26dev(i,j),ff27dev(i,j),ff28dev(i,j),ff29dev(i,j),ff30dev(i,j),&
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j),&
    !OUT:
    k1dev(i,j),k2dev(i,j),Nhdev(i,j),KKdev(i,j))
   ! 
   k1(i,j)=k1dev(i,j)-Ak1(i,j)
   k2(i,j)=k2dev(i,j)-Ak2(i,j)
   Nh(i,j)=Nhdev(i,j)-ANh(i,j)
   KK(i,j)=KKdev(i,j)-AKK(i,j)
END DO

!******* pochodne fkcji k1,k2,Nh,KK na siatce (x,y), bez pktow brzegowych - dewiacje *******
CALL Pochodne(k1,k2,Nh,KK,&
 !OUT:
 dxNh,dyNh,dxxNh,dyyNh,dxyNh,dxKK,dyKK,dxk1,dyk1,dxk2,dyk2)


!* SPR *! wypisanie norm dla fkcji k1,k2,Nh,KK do pliku norms.dat ----------------------------------------------------------
!* norma L2-Euklidesowa
L2k1=0.D0
L2k2=0.D0
L2Nh=0.D0
L2KK=0.D0
do i=1,Nx
 do j=1,Ny
    L2k1=L2k1+k1(i,j)*k1(i,j)
    L2k2=L2k2+k2(i,j)*k2(i,j)
    L2Nh=L2Nh+Nh(i,j)*Nh(i,j)
    L2KK=L2KK+KK(i,j)*KK(i,j)
 end do
end do
!
!* norma infty
Infk1=0.D0
Infk2=0.D0
InfNh=0.D0
InfKK=0.D0
do j=1,Ny
   Infk1=Infk1+dabs(k1(:,j))
   Infk2=Infk2+dabs(k2(:,j))
   InfNh=InfNh+dabs(Nh(:,j))
   InfKK=InfKK+dabs(KK(:,j))
end do
!
write(99,13) z,dsqrt(L2k1),dsqrt(L2k2),dsqrt(L2Nh),dsqrt(L2KK),maxval(Infk1),maxval(Infk2),maxval(InfNh),maxval(InfKK),&
 maxval(dabs(k1)),maxval(dabs(k2)),maxval(dabs(Nh)),maxval(dabs(KK))

!* SPR *! normy xyz ----------------------------------------------------------------------------------------------------------
if (z.gt.1.D0) then
   L2xyz1k1=L2xyz1k1+L2k1
   L2xyz1k2=L2xyz1k2+L2k2
   L2xyz1Nh=L2xyz1Nh+L2Nh
   L2xyz1KK=L2xyz1KK+L2KK
   !*
   if (maxval(dabs(k1)).gt.max1k1) max1k1=maxval(dabs(k1))
   if (maxval(dabs(k2)).gt.max1k2) max1k2=maxval(dabs(k2))
   if (maxval(dabs(Nh)).gt.max1Nh) max1Nh=maxval(dabs(Nh))
   if (maxval(dabs(KK)).gt.max1KK) max1KK=maxval(dabs(KK))
end if
if (z.gt.0.5D0) then
   L2xyz05k1=L2xyz05k1+L2k1
   L2xyz05k2=L2xyz05k2+L2k2
   L2xyz05Nh=L2xyz05Nh+L2Nh
   L2xyz05KK=L2xyz05KK+L2KK
   !*
   if (maxval(dabs(k1)).gt.max05k1) max05k1=maxval(dabs(k1))
   if (maxval(dabs(k2)).gt.max05k2) max05k2=maxval(dabs(k2))
   if (maxval(dabs(Nh)).gt.max05Nh) max05Nh=maxval(dabs(Nh))
   if (maxval(dabs(KK)).gt.max05KK) max05KK=maxval(dabs(KK))
end if
if (z.gt.0.25D0) then
   L2xyz025k1=L2xyz025k1+L2k1
   L2xyz025k2=L2xyz025k2+L2k2
   L2xyz025Nh=L2xyz025Nh+L2Nh
   L2xyz025KK=L2xyz025KK+L2KK
   !*
   if (maxval(dabs(k1)).gt.max025k1) max025k1=maxval(dabs(k1))
   if (maxval(dabs(k2)).gt.max025k2) max025k2=maxval(dabs(k2))
   if (maxval(dabs(Nh)).gt.max025Nh) max025Nh=maxval(dabs(Nh))
   if (maxval(dabs(KK)).gt.max025KK) max025KK=maxval(dabs(KK))
end if
if (z.gt.0.1D0) then
   L2xyz01k1=L2xyz01k1+L2k1
   L2xyz01k2=L2xyz01k2+L2k2
   L2xyz01Nh=L2xyz01Nh+L2Nh
   L2xyz01KK=L2xyz01KK+L2KK
   !*
   if (maxval(dabs(k1)).gt.max01k1) max01k1=maxval(dabs(k1))
   if (maxval(dabs(k2)).gt.max01k2) max01k2=maxval(dabs(k2))
   if (maxval(dabs(Nh)).gt.max01Nh) max01Nh=maxval(dabs(Nh))
   if (maxval(dabs(KK)).gt.max01KK) max01KK=maxval(dabs(KK))
end if

!* SPR *! wypisanie fkcji k1,k2,Nh,KK,(A)k1+k1,(A)k2+k2,(A)Nh+Nh,(A)KK+KK do plikow z.dat ------------------------------------
!* 2D
if (LICZz.eq.INT((DABS(U)-5.D0)/DABS(dz))) then   !5
 do i=2,Nx-1
  do j=2,Ny-1
     write(31,11) x(i),y(j),z,k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(31,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-2.5D0)/DABS(dz))) then   !2.5
 do i=2,Nx-1
  do j=2,Ny-1
     write(32,11) x(i),y(j),z,k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(32,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-2.D0)/DABS(dz))) then   !2
 do i=2,Nx-1
  do j=2,Ny-1
     write(33,11) x(i),y(j),z,k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(33,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-1.D0)/DABS(dz))) then   !1
 do i=2,Nx-1
  do j=2,Ny-1
     write(34,11) x(i),y(j),z,k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(34,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-0.5D0)/DABS(dz))) then   !0.5
 do i=2,Nx-1
  do j=2,Ny-1
     write(35,11) x(i),y(j),z,k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(35,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-0.25D0)/DABS(dz))) then   !0.25
 do i=2,Nx-1
  do j=2,Ny-1
     write(36,11) x(i),y(j),z,k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(36,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-0.1D0)/DABS(dz))) then   !0.1
 do i=2,Nx-1
  do j=2,Ny-1
     write(37,11) x(i),y(j),z,k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
  end do
  write(37,*)
 end do
end if
if (LICZz.eq.INT((DABS(U)-0.05D0)/DABS(dz))) then   !0.05
 do i=2,Nx-1
  do j=2,Ny-1
     write(38,11) x(i),y(j),z,k1(i,j),k2(i,j),Nh(i,j),KK(i,j),Ak1(i,j)+k1(i,j),Ak2(i,j)+k2(i,j),ANh(i,j)+Nh(i,j),AKK(i,j)+KK(i,j)
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
     !
     CALL VarFuncdev(H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
      H1xdev(i,j),H1xxdev(i,j),H1xydev(i,j),H1xzdev(i,j),H1ydev(i,j),H1yydev(i,j),H1yzdev(i,j),H1zdev(i,j),H1zzdev(i,j),&
      H2dev(i,j),H2tdev(i,j),H2ttdev(i,j),H2txdev(i,j),H2tydev(i,j),H2tzdev(i,j),&
      H2xdev(i,j),H2xxdev(i,j),H2xydev(i,j),H2xzdev(i,j),H2ydev(i,j),H2yydev(i,j),H2yzdev(i,j),H2zdev(i,j),H2zzdev(i,j),&
      H1H2dev(i,j),&
      !
      l11dev(i,j),l11tdev(i,j),l11ttdev(i,j),l11txdev(i,j),l11tydev(i,j),l11tzdev(i,j),&
      l11xdev(i,j),l11xxdev(i,j),l11xydev(i,j),l11xzdev(i,j),&
      l11ydev(i,j),l11yydev(i,j),l11yzdev(i,j),l11zdev(i,j),l11zzdev(i,j),&
      l12dev(i,j),l12tdev(i,j),l12ttdev(i,j),l12txdev(i,j),l12tydev(i,j),l12tzdev(i,j),&
      l12xdev(i,j),l12xxdev(i,j),l12xydev(i,j),l12xzdev(i,j),&
      l12ydev(i,j),l12yydev(i,j),l12yzdev(i,j),l12zdev(i,j),l12zzdev(i,j),&
      l13dev(i,j),l13tdev(i,j),l13ttdev(i,j),l13txdev(i,j),l13tydev(i,j),l13tzdev(i,j),&
      l13xdev(i,j),l13xxdev(i,j),l13xydev(i,j),l13xzdev(i,j),&
      l13ydev(i,j),l13yydev(i,j),l13yzdev(i,j),l13zdev(i,j),l13zzdev(i,j),&
      l21dev(i,j),l21tdev(i,j),l21ttdev(i,j),l21txdev(i,j),l21tydev(i,j),l21tzdev(i,j),&
      l21xdev(i,j),l21xxdev(i,j),l21xydev(i,j),l21xzdev(i,j),&
      l21ydev(i,j),l21yydev(i,j),l21yzdev(i,j),l21zdev(i,j),l21zzdev(i,j),&
      l22dev(i,j),l22tdev(i,j),l22ttdev(i,j),l22txdev(i,j),l22tydev(i,j),l22tzdev(i,j),&
      l22xdev(i,j),l22xxdev(i,j),l22xydev(i,j),l22xzdev(i,j),&
      l22ydev(i,j),l22yydev(i,j),l22yzdev(i,j),l22zdev(i,j),l22zzdev(i,j),&
      l23dev(i,j),l23tdev(i,j),l23ttdev(i,j),l23txdev(i,j),l23tydev(i,j),l23tzdev(i,j),&
      l23xdev(i,j),l23xxdev(i,j),l23xydev(i,j),l23xzdev(i,j),&
      l23ydev(i,j),l23yydev(i,j),l23yzdev(i,j),l23zdev(i,j),l23zzdev(i,j),&
      !
      l11n1dev(i,j),l21n2dev(i,j),l11n12dev(i,j),l21n22dev(i,j),l112dev(i,j),l122dev(i,j),l132dev(i,j),&
      l212dev(i,j),l222dev(i,j),l232dev(i,j),&
      !
      f1dev(i,j),f2dev(i,j),f3dev(i,j),f4dev(i,j),f5dev(i,j),f6dev(i,j),f7dev(i,j),f8dev(i,j),f9dev(i,j),f10dev(i,j),&
      f11dev(i,j),f12dev(i,j),f13dev(i,j),f14dev(i,j),f15dev(i,j),f16dev(i,j),f17dev(i,j),f18dev(i,j),f19dev(i,j),f20dev(i,j),&
      f21dev(i,j),f22dev(i,j),f23dev(i,j),f24dev(i,j),f25dev(i,j),f26dev(i,j),f27dev(i,j),f28dev(i,j),f29dev(i,j),f30dev(i,j),&
      f31dev(i,j),f32dev(i,j),f33dev(i,j),f34dev(i,j),f35dev(i,j),f36dev(i,j),f37dev(i,j),f38dev(i,j),f39dev(i,j),f40dev(i,j),&
      f41dev(i,j),f42dev(i,j),f43dev(i,j),f44dev(i,j),f45dev(i,j),f46dev(i,j),f47dev(i,j),f48dev(i,j),f49dev(i,j),f50dev(i,j),&
      f51dev(i,j),f52dev(i,j),f53dev(i,j),f54dev(i,j),f55dev(i,j),f56dev(i,j),f57dev(i,j),f58dev(i,j),f59dev(i,j),f60dev(i,j),&
      f61dev(i,j),f62dev(i,j),f63dev(i,j),f64dev(i,j),f65dev(i,j),f66dev(i,j),f67dev(i,j),f68dev(i,j),f69dev(i,j),f70dev(i,j),&
      f71dev(i,j),f72dev(i,j),f73dev(i,j),f74dev(i,j),f75dev(i,j),f76dev(i,j),f77dev(i,j),f78dev(i,j),f79dev(i,j),f80dev(i,j),&
      f81dev(i,j),f82dev(i,j),f83dev(i,j),f84dev(i,j),f85dev(i,j),f86dev(i,j),f87dev(i,j),f88dev(i,j),f89dev(i,j),f90dev(i,j),&
      f91dev(i,j),f92dev(i,j),f93dev(i,j),f94dev(i,j),f95dev(i,j),f96dev(i,j),f97dev(i,j),f98dev(i,j),f99dev(i,j),f100dev(i,j),&
      f101dev(i,j),f102dev(i,j),f103dev(i,j),f104dev(i,j),f105dev(i,j),&
      f106dev(i,j),f107dev(i,j),f108dev(i,j),f109dev(i,j),f110dev(i,j),&
      f111dev(i,j),f112dev(i,j),f113dev(i,j),f114dev(i,j),f115dev(i,j),f116dev(i,j),f117dev(i,j),f118dev(i,j),f119dev(i,j),&
      !
      ff1dev(i,j),ff2dev(i,j),ff3dev(i,j),ff4dev(i,j),ff5dev(i,j),ff6dev(i,j),ff7dev(i,j),ff8dev(i,j),ff9dev(i,j),ff10dev(i,j),&
      ff11dev(i,j),ff12dev(i,j),ff13dev(i,j),ff14dev(i,j),ff15dev(i,j),&
      ff16dev(i,j),ff17dev(i,j),ff18dev(i,j),ff19dev(i,j),ff20dev(i,j),&
      ff21dev(i,j),ff22dev(i,j),ff23dev(i,j),ff24dev(i,j),ff25dev(i,j),&
      ff26dev(i,j),ff27dev(i,j),ff28dev(i,j),ff29dev(i,j),ff30dev(i,j),&
      ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j),&
      !OUT:
      G1dev,G2dev,G3dev,G4dev,G5dev,G6dev,G7dev,G8dev,dxG1dev,dyG1dev,dxG2dev,dyG2dev,&
      adev,ddev,kappadev,Kstardev,dxkappadev,dykappadev)
     !
     write(70,99) z,x(i),y(j),(ANh(i,j)+Nh(i,j))*DSQRT(d(i,j)),AKK(i,j)+KK(i,j)+kappa(i,j),&
      Nh(i,j)*DSQRT(d(i,j))+(ANh(i,j)+Nh(i,j))*(DSQRT(ddev)-DSQRT(d(i,j))),KK(i,j)+(kappadev-kappa(i,j)),&
      Nh(i,j)*DSQRT(d(i,j)),KK(i,j)
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

!* SPR *! wypisanie norm xyz do pliku Nxyz.dat -------------------------------------------------------------------------------
write(62,99) z,dsqrt(L2xyz1k1),dsqrt(L2xyz1k2),dsqrt(L2xyz1Nh),dsqrt(L2xyz1KK),max1k1,max1k2,max1Nh,max1KK
write(62,99) z,dsqrt(L2xyz05k1),dsqrt(L2xyz05k2),dsqrt(L2xyz05Nh),dsqrt(L2xyz05KK),max05k1,max05k2,max05Nh,max05KK
write(62,99) z,dsqrt(L2xyz025k1),dsqrt(L2xyz025k2),dsqrt(L2xyz025Nh),dsqrt(L2xyz025KK),max025k1,max025k2,max025Nh,max025KK
write(62,99) z,dsqrt(L2xyz01k1),dsqrt(L2xyz01k2),dsqrt(L2xyz01Nh),dsqrt(L2xyz01KK),max01k1,max01k2,max01Nh,max01KK
!* *! ------------------------------------------------------------------------------------------------------------------------


DEALLOCATE(x,y,k1,k2,Nh,KK,dxNh,dyNh,dxxNh,dyyNh,dxyNh,dxKK,dyKK,dxk1,dyk1,dxk2,dyk2,&
 Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxxNh,AdyyNh,AdxyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,&
 k1dev,k2dev,Nhdev,KKdev,&
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
 H1dev,H1tdev,H1ttdev,H1txdev,H1tydev,H1tzdev,H1xdev,H1xxdev,H1xydev,H1xzdev,H1ydev,H1yydev,H1yzdev,H1zdev,H1zzdev,&
 H2dev,H2tdev,H2ttdev,H2txdev,H2tydev,H2tzdev,H2xdev,H2xxdev,H2xydev,H2xzdev,H2ydev,H2yydev,H2yzdev,H2zdev,H2zzdev,H1H2dev,&
 l11dev,l11tdev,l11ttdev,l11txdev,l11tydev,l11tzdev,l11xdev,l11xxdev,l11xydev,l11xzdev,l11ydev,l11yydev,l11yzdev,l11zdev,l11zzdev,&
 l12dev,l12tdev,l12ttdev,l12txdev,l12tydev,l12tzdev,l12xdev,l12xxdev,l12xydev,l12xzdev,l12ydev,l12yydev,l12yzdev,l12zdev,l12zzdev,&
 l13dev,l13tdev,l13ttdev,l13txdev,l13tydev,l13tzdev,l13xdev,l13xxdev,l13xydev,l13xzdev,l13ydev,l13yydev,l13yzdev,l13zdev,l13zzdev,&
 l21dev,l21tdev,l21ttdev,l21txdev,l21tydev,l21tzdev,l21xdev,l21xxdev,l21xydev,l21xzdev,l21ydev,l21yydev,l21yzdev,l21zdev,l21zzdev,&
 l22dev,l22tdev,l22ttdev,l22txdev,l22tydev,l22tzdev,l22xdev,l22xxdev,l22xydev,l22xzdev,l22ydev,l22yydev,l22yzdev,l22zdev,l22zzdev,&
 l23dev,l23tdev,l23ttdev,l23txdev,l23tydev,l23tzdev,l23xdev,l23xxdev,l23xydev,l23xzdev,l23ydev,l23yydev,l23yzdev,l23zdev,l23zzdev,&
 l11n1dev,l21n2dev,l11n12dev,l21n22dev,l112dev,l122dev,l132dev,l212dev,l222dev,l232dev,&
 f1dev,f2dev,f3dev,f4dev,f5dev,f6dev,f7dev,f8dev,f9dev,f10dev,f11dev,f12dev,f13dev,f14dev,f15dev,&
 f16dev,f17dev,f18dev,f19dev,f20dev,f21dev,f22dev,f23dev,f24dev,f25dev,f26dev,f27dev,f28dev,f29dev,f30dev,&
 f31dev,f32dev,f33dev,f34dev,f35dev,f36dev,f37dev,f38dev,f39dev,f40dev,f41dev,f42dev,f43dev,f44dev,f45dev,&
 f46dev,f47dev,f48dev,f49dev,f50dev,f51dev,f52dev,f53dev,f54dev,f55dev,f56dev,f57dev,f58dev,f59dev,f60dev,&
 f61dev,f62dev,f63dev,f64dev,f65dev,f66dev,f67dev,f68dev,f69dev,f70dev,f71dev,f72dev,f73dev,f74dev,f75dev,&
 f76dev,f77dev,f78dev,f79dev,f80dev,f81dev,f82dev,f83dev,f84dev,f85dev,f86dev,f87dev,f88dev,f89dev,f90dev,&
 f91dev,f92dev,f93dev,f94dev,f95dev,f96dev,f97dev,f98dev,f99dev,f100dev,f101dev,f102dev,f103dev,f104dev,f105dev,&
 f106dev,f107dev,f108dev,f109dev,f110dev,f111dev,f112dev,f113dev,f114dev,f115dev,f116dev,f117dev,f118dev,f119dev,&
 ff1dev,ff2dev,ff3dev,ff4dev,ff5dev,ff6dev,ff7dev,ff8dev,ff9dev,ff10dev,ff11dev,ff12dev,ff13dev,ff14dev,ff15dev,&
 ff16dev,ff17dev,ff18dev,ff19dev,ff20dev,ff21dev,ff22dev,ff23dev,ff24dev,ff25dev,ff26dev,ff27dev,ff28dev,ff29dev,ff30dev,&
 ff31dev,ff32dev,ff33dev,ff34dev,ff35dev,&
 G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,dxkappa,dykappa,G1pz,G2pz,G3pz,G4pz,G5pz,G6pz,G7pz,G8pz,&
 dxG1pz,dyG1pz,dxG2pz,dyG2pz,apz,dpz,kappapz,Kstarpz,dxkappapz,dykappapz,&
 Ak1pz,Ak2pz,ANhpz,AKKpz,AdxNhpz,AdyNhpz,AdxxNhpz,AdyyNhpz,AdxyNhpz,AdxKKpz,AdyKKpz,Adxk1pz,Adyk1pz,Adxk2pz,Adyk2pz,&
 dzRK4KK,dzRK4k1,dzRK4k2,dzRK4Nh,&
 Nhupx,Nhupy,Kcxx,Kcyy,Kcxy)


11 FORMAT(11(1X,F20.15))
13 FORMAT(13(1X,F20.15))
94 FORMAT(4(1X,F20.15))
99 FORMAT(9(1X,F20.15))

CLOSE(1)
CLOSE(2)
CLOSE(3)

CLOSE(99)

CLOSE(31)
CLOSE(32)
CLOSE(33)
CLOSE(34)
CLOSE(35)
CLOSE(36)
CLOSE(37)

CLOSE(40)

CLOSE(62)

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
