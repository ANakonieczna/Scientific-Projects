MODULE module222dzRK4

CONTAINS
SUBROUTINE dzRK4(x,y,zpz,&
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
USE module001FunctionsVarHl
USE module002AnalDerivs
USE module002FunctionsEvAll
USE module002FunctionsVar
USE module111Pochodne
USE module222k1234
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy
INTEGER :: Nx,Ny
REAL(8) :: p2
COMMON /PARAMS/ M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy,Nx,Ny,p2
!*************************
REAL(8),DIMENSION(:),INTENT(IN) :: x,y
REAL(8),INTENT(IN) :: zpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: H1pz,H1tpz,H1ttpz,H1txpz,H1typz,H1tzpz,H1xpz,H1xxpz,H1xypz,H1xzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: H1ypz,H1yypz,H1yzpz,H1zpz,H1zzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: H2pz,H2tpz,H2ttpz,H2txpz,H2typz,H2tzpz,H2xpz,H2xxpz,H2xypz,H2xzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: H2ypz,H2yypz,H2yzpz,H2zpz,H2zzpz,H1H2pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l11pz,l11tpz,l11ttpz,l11txpz,l11typz,l11tzpz,l11xpz,l11xxpz,l11xypz,l11xzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l11ypz,l11yypz,l11yzpz,l11zpz,l11zzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l12pz,l12tpz,l12ttpz,l12txpz,l12typz,l12tzpz,l12xpz,l12xxpz,l12xypz,l12xzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l12ypz,l12yypz,l12yzpz,l12zpz,l12zzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l13pz,l13tpz,l13ttpz,l13txpz,l13typz,l13tzpz,l13xpz,l13xxpz,l13xypz,l13xzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l13ypz,l13yypz,l13yzpz,l13zpz,l13zzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l21pz,l21tpz,l21ttpz,l21txpz,l21typz,l21tzpz,l21xpz,l21xxpz,l21xypz,l21xzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l21ypz,l21yypz,l21yzpz,l21zpz,l21zzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l22pz,l22tpz,l22ttpz,l22txpz,l22typz,l22tzpz,l22xpz,l22xxpz,l22xypz,l22xzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l22ypz,l22yypz,l22yzpz,l22zpz,l22zzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l23pz,l23tpz,l23ttpz,l23txpz,l23typz,l23tzpz,l23xpz,l23xxpz,l23xypz,l23xzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l23ypz,l23yypz,l23yzpz,l23zpz,l23zzpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: l11n1pz,l21n2pz,l11n12pz,l21n22pz,l112pz,l122pz,l132pz,l212pz,l222pz,l232pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: f1pz,f2pz,f3pz,f4pz,f5pz,f6pz,f7pz,f8pz,f9pz,f10pz,f11pz,f12pz,f13pz,f14pz,f15pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: f16pz,f17pz,f18pz,f19pz,f20pz,f21pz,f22pz,f23pz,f24pz,f25pz,f26pz,f27pz,f28pz,f29pz,f30pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: f31pz,f32pz,f33pz,f34pz,f35pz,f36pz,f37pz,f38pz,f39pz,f40pz,f41pz,f42pz,f43pz,f44pz,f45pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: f46pz,f47pz,f48pz,f49pz,f50pz,f51pz,f52pz,f53pz,f54pz,f55pz,f56pz,f57pz,f58pz,f59pz,f60pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: f61pz,f62pz,f63pz,f64pz,f65pz,f66pz,f67pz,f68pz,f69pz,f70pz,f71pz,f72pz,f73pz,f74pz,f75pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: f76pz,f77pz,f78pz,f79pz,f80pz,f81pz,f82pz,f83pz,f84pz,f85pz,f86pz,f87pz,f88pz,f89pz,f90pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: f91pz,f92pz,f93pz,f94pz,f95pz,f96pz,f97pz,f98pz,f99pz,f100pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: f101pz,f102pz,f103pz,f104pz,f105pz,f106pz,f107pz,f108pz,f109pz,f110pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: f111pz,f112pz,f113pz,f114pz,f115pz,f116pz,f117pz,f118pz,f119pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: ff1pz,ff2pz,ff3pz,ff4pz,ff5pz,ff6pz,ff7pz,ff8pz,ff9pz,ff10pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: ff11pz,ff12pz,ff13pz,ff14pz,ff15pz,ff16pz,ff17pz,ff18pz,ff19pz,ff20pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: ff21pz,ff22pz,ff23pz,ff24pz,ff25pz,ff26pz,ff27pz,ff28pz,ff29pz,ff30pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: ff31pz,ff32pz,ff33pz,ff34pz,ff35pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: G1pz,G2pz,G3pz,G4pz,G5pz,G6pz,G7pz,G8pz,dxG1pz,dyG1pz,dxG2pz,dyG2pz,apz,dpz,kappapz,Kstarpz
REAL(8),DIMENSION(:,:),INTENT(IN) :: k1,k2,Nh,KK
REAL(8),DIMENSION(:,:),INTENT(IN) :: dxNh,dyNh,dxKK,dyKK,dxkappapz,dykappapz,dxk1,dyk1,dxk2,dyk2,dxxNh,dyyNh,dxyNh
REAL(8),DIMENSION(:,:),INTENT(IN) :: Ak1pz,Ak2pz,ANhpz,AKKpz,AdxNhpz,AdyNhpz,AdxKKpz,AdyKKpz,Adxk1pz,Adyk1pz,Adxk2pz,Adyk2pz
REAL(8),DIMENSION(:,:),INTENT(IN) :: AdxxNhpz,AdyyNhpz,AdxyNhpz
REAL(8),INTENT(IN) :: z
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
REAL(8),DIMENSION(:,:),INTENT(IN) :: G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,dxkappa,dykappa
REAL(8),DIMENSION(:,:),INTENT(IN) :: Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh
REAL(8),DIMENSION(:,:),INTENT(OUT) :: dzRK4KK,dzRK4k1,dzRK4k2,dzRK4Nh
!
INTEGER :: i,j
REAL(8) :: zmod
!
REAL(8),DIMENSION(Nx,Ny) :: k1KK,k2KK,k3KK,k4KK,k1k1,k2k1,k3k1,k4k1,k1k2,k2k2,k3k2,k4k2,k1Nh,k2Nh,k3Nh,k4Nh
REAL(8),DIMENSION(Nx,Ny) :: H1_mod,H1t_mod,H1tt_mod,H1tx_mod,H1ty_mod,H1tz_mod,H1x_mod,H1xx_mod,H1xy_mod,H1xz_mod
REAL(8),DIMENSION(Nx,Ny) :: H1y_mod,H1yy_mod,H1yz_mod,H1z_mod,H1zz_mod
REAL(8),DIMENSION(Nx,Ny) :: H2_mod,H2t_mod,H2tt_mod,H2tx_mod,H2ty_mod,H2tz_mod,H2x_mod,H2xx_mod,H2xy_mod,H2xz_mod
REAL(8),DIMENSION(Nx,Ny) :: H2y_mod,H2yy_mod,H2yz_mod,H2z_mod,H2zz_mod,H1H2_mod
REAL(8),DIMENSION(Nx,Ny) :: l11_mod,l11t_mod,l11tt_mod,l11tx_mod,l11ty_mod,l11tz_mod,l11x_mod,l11xx_mod,l11xy_mod,l11xz_mod
REAL(8),DIMENSION(Nx,Ny) :: l11y_mod,l11yy_mod,l11yz_mod,l11z_mod,l11zz_mod
REAL(8),DIMENSION(Nx,Ny) :: l12_mod,l12t_mod,l12tt_mod,l12tx_mod,l12ty_mod,l12tz_mod,l12x_mod,l12xx_mod,l12xy_mod,l12xz_mod
REAL(8),DIMENSION(Nx,Ny) :: l12y_mod,l12yy_mod,l12yz_mod,l12z_mod,l12zz_mod
REAL(8),DIMENSION(Nx,Ny) :: l13_mod,l13t_mod,l13tt_mod,l13tx_mod,l13ty_mod,l13tz_mod,l13x_mod,l13xx_mod,l13xy_mod,l13xz_mod
REAL(8),DIMENSION(Nx,Ny) :: l13y_mod,l13yy_mod,l13yz_mod,l13z_mod,l13zz_mod
REAL(8),DIMENSION(Nx,Ny) :: l21_mod,l21t_mod,l21tt_mod,l21tx_mod,l21ty_mod,l21tz_mod,l21x_mod,l21xx_mod,l21xy_mod,l21xz_mod
REAL(8),DIMENSION(Nx,Ny) :: l21y_mod,l21yy_mod,l21yz_mod,l21z_mod,l21zz_mod
REAL(8),DIMENSION(Nx,Ny) :: l22_mod,l22t_mod,l22tt_mod,l22tx_mod,l22ty_mod,l22tz_mod,l22x_mod,l22xx_mod,l22xy_mod,l22xz_mod
REAL(8),DIMENSION(Nx,Ny) :: l22y_mod,l22yy_mod,l22yz_mod,l22z_mod,l22zz_mod
REAL(8),DIMENSION(Nx,Ny) :: l23_mod,l23t_mod,l23tt_mod,l23tx_mod,l23ty_mod,l23tz_mod,l23x_mod,l23xx_mod,l23xy_mod,l23xz_mod
REAL(8),DIMENSION(Nx,Ny) :: l23y_mod,l23yy_mod,l23yz_mod,l23z_mod,l23zz_mod
REAL(8),DIMENSION(Nx,Ny) :: l11n1_mod,l21n2_mod,l11n12_mod,l21n22_mod,l112_mod,l122_mod,l132_mod,l212_mod,l222_mod,l232_mod
REAL(8),DIMENSION(Nx,Ny) :: f1_mod,f2_mod,f3_mod,f4_mod,f5_mod,f6_mod,f7_mod,f8_mod,f9_mod,f10_mod
REAL(8),DIMENSION(Nx,Ny) :: f11_mod,f12_mod,f13_mod,f14_mod,f15_mod,f16_mod,f17_mod,f18_mod,f19_mod,f20_mod
REAL(8),DIMENSION(Nx,Ny) :: f21_mod,f22_mod,f23_mod,f24_mod,f25_mod,f26_mod,f27_mod,f28_mod,f29_mod,f30_mod
REAL(8),DIMENSION(Nx,Ny) :: f31_mod,f32_mod,f33_mod,f34_mod,f35_mod,f36_mod,f37_mod,f38_mod,f39_mod,f40_mod
REAL(8),DIMENSION(Nx,Ny) :: f41_mod,f42_mod,f43_mod,f44_mod,f45_mod,f46_mod,f47_mod,f48_mod,f49_mod,f50_mod
REAL(8),DIMENSION(Nx,Ny) :: f51_mod,f52_mod,f53_mod,f54_mod,f55_mod,f56_mod,f57_mod,f58_mod,f59_mod,f60_mod
REAL(8),DIMENSION(Nx,Ny) :: f61_mod,f62_mod,f63_mod,f64_mod,f65_mod,f66_mod,f67_mod,f68_mod,f69_mod,f70_mod
REAL(8),DIMENSION(Nx,Ny) :: f71_mod,f72_mod,f73_mod,f74_mod,f75_mod,f76_mod,f77_mod,f78_mod,f79_mod,f80_mod
REAL(8),DIMENSION(Nx,Ny) :: f81_mod,f82_mod,f83_mod,f84_mod,f85_mod,f86_mod,f87_mod,f88_mod,f89_mod,f90_mod
REAL(8),DIMENSION(Nx,Ny) :: f91_mod,f92_mod,f93_mod,f94_mod,f95_mod,f96_mod,f97_mod,f98_mod,f99_mod,f100_mod
REAL(8),DIMENSION(Nx,Ny) :: f101_mod,f102_mod,f103_mod,f104_mod,f105_mod,f106_mod,f107_mod,f108_mod,f109_mod,f110_mod
REAL(8),DIMENSION(Nx,Ny) :: f111_mod,f112_mod,f113_mod,f114_mod,f115_mod,f116_mod,f117_mod,f118_mod,f119_mod
REAL(8),DIMENSION(Nx,Ny) :: ff1_mod,ff2_mod,ff3_mod,ff4_mod,ff5_mod,ff6_mod,ff7_mod,ff8_mod,ff9_mod,ff10_mod
REAL(8),DIMENSION(Nx,Ny) :: ff11_mod,ff12_mod,ff13_mod,ff14_mod,ff15_mod,ff16_mod,ff17_mod,ff18_mod,ff19_mod,ff20_mod
REAL(8),DIMENSION(Nx,Ny) :: ff21_mod,ff22_mod,ff23_mod,ff24_mod,ff25_mod,ff26_mod,ff27_mod,ff28_mod,ff29_mod,ff30_mod
REAL(8),DIMENSION(Nx,Ny) :: ff31_mod,ff32_mod,ff33_mod,ff34_mod,ff35_mod
REAL(8),DIMENSION(Nx,Ny) :: G1_mod,G2_mod,G3_mod,G4_mod,G5_mod,G6_mod,G7_mod,G8_mod,dxG1_mod,dyG1_mod,dxG2_mod,dyG2_mod
REAL(8),DIMENSION(Nx,Ny) :: a_mod,d_mod,kappa_mod,Kstar_mod,dxkappa_mod,dykappa_mod
REAL(8),DIMENSION(Nx,Ny) :: k1mod,k2mod,Nhmod,KKmod
REAL(8),DIMENSION(Nx,Ny) :: dxNhmod,dyNhmod,dxKKmod,dyKKmod,dxk1mod,dyk1mod,dxk2mod,dyk2mod,dxxNhmod,dyyNhmod,dxyNhmod
REAL(8),DIMENSION(Nx,Ny) :: Ak1_mod,Ak2_mod,ANh_mod,AKK_mod,AdxNh_mod,AdyNh_mod,AdxKK_mod,AdyKK_mod
REAL(8),DIMENSION(Nx,Ny) :: Adxk1_mod,Adyk1_mod,Adxk2_mod,Adyk2_mod,AdxxNh_mod,AdyyNh_mod,AdxyNh_mod
REAL(8),DIMENSION(Nx,Ny) :: Adzk1_mod,Adzk2_mod,AdzNh_mod,AdzKK_mod,Adxxk1_mod,Adyyk1_mod,Adxyk1_mod
REAL(8),DIMENSION(Nx,Ny) :: Adxxk2_mod,Adyyk2_mod,Adxyk2_mod,AdxxKK_mod,AdyyKK_mod,AdxyKK_mod


!******* zmodyfikowane z, do RK4-k2 i RK4-k3 *******
zmod=zpz+0.5D0*dz


!******* RK4-k1 na siatce (x,y) *******
CALL k1234(H1pz,H1tpz,H1ttpz,H1txpz,H1typz,H1tzpz,H1xpz,H1xxpz,H1xypz,H1xzpz,H1ypz,H1yypz,H1yzpz,H1zpz,H1zzpz,&
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
 !OUT:
 k1KK,k1k1,k1k2,k1Nh)


!* do RK4-k2 *****************************************************************************************************************
!******* fkcje pomocnicze H i l na siatce (x,y), z uwzglednieniem zmod *******
DO i=1,Nx
 DO j=1,Ny
    CALL VarFuncHl(x(i),y(j),zmod,&
     !OUT:
     H1_mod(i,j),H1t_mod(i,j),H1tt_mod(i,j),H1tx_mod(i,j),H1ty_mod(i,j),H1tz_mod(i,j),&
     H1x_mod(i,j),H1xx_mod(i,j),H1xy_mod(i,j),H1xz_mod(i,j),H1y_mod(i,j),H1yy_mod(i,j),H1yz_mod(i,j),&
     H1z_mod(i,j),H1zz_mod(i,j),&
     H2_mod(i,j),H2t_mod(i,j),H2tt_mod(i,j),H2tx_mod(i,j),H2ty_mod(i,j),H2tz_mod(i,j),&
     H2x_mod(i,j),H2xx_mod(i,j),H2xy_mod(i,j),H2xz_mod(i,j),H2y_mod(i,j),H2yy_mod(i,j),H2yz_mod(i,j),&
     H2z_mod(i,j),H2zz_mod(i,j),H1H2_mod(i,j),&
     !
     l11_mod(i,j),l11t_mod(i,j),l11tt_mod(i,j),l11tx_mod(i,j),l11ty_mod(i,j),l11tz_mod(i,j),&
     l11x_mod(i,j),l11xx_mod(i,j),l11xy_mod(i,j),l11xz_mod(i,j),l11y_mod(i,j),l11yy_mod(i,j),l11yz_mod(i,j),&
     l11z_mod(i,j),l11zz_mod(i,j),&
     l12_mod(i,j),l12t_mod(i,j),l12tt_mod(i,j),l12tx_mod(i,j),l12ty_mod(i,j),l12tz_mod(i,j),&
     l12x_mod(i,j),l12xx_mod(i,j),l12xy_mod(i,j),l12xz_mod(i,j),l12y_mod(i,j),l12yy_mod(i,j),l12yz_mod(i,j),&
     l12z_mod(i,j),l12zz_mod(i,j),&
     l13_mod(i,j),l13t_mod(i,j),l13tt_mod(i,j),l13tx_mod(i,j),l13ty_mod(i,j),l13tz_mod(i,j),&
     l13x_mod(i,j),l13xx_mod(i,j),l13xy_mod(i,j),l13xz_mod(i,j),l13y_mod(i,j),l13yy_mod(i,j),l13yz_mod(i,j),&
     l13z_mod(i,j),l13zz_mod(i,j),&
     l21_mod(i,j),l21t_mod(i,j),l21tt_mod(i,j),l21tx_mod(i,j),l21ty_mod(i,j),l21tz_mod(i,j),&
     l21x_mod(i,j),l21xx_mod(i,j),l21xy_mod(i,j),l21xz_mod(i,j),l21y_mod(i,j),l21yy_mod(i,j),l21yz_mod(i,j),&
     l21z_mod(i,j),l21zz_mod(i,j),&
     l22_mod(i,j),l22t_mod(i,j),l22tt_mod(i,j),l22tx_mod(i,j),l22ty_mod(i,j),l22tz_mod(i,j),&
     l22x_mod(i,j),l22xx_mod(i,j),l22xy_mod(i,j),l22xz_mod(i,j),l22y_mod(i,j),l22yy_mod(i,j),l22yz_mod(i,j),&
     l22z_mod(i,j),l22zz_mod(i,j),&
     l23_mod(i,j),l23t_mod(i,j),l23tt_mod(i,j),l23tx_mod(i,j),l23ty_mod(i,j),l23tz_mod(i,j),&
     l23x_mod(i,j),l23xx_mod(i,j),l23xy_mod(i,j),l23xz_mod(i,j),l23y_mod(i,j),l23yy_mod(i,j),l23yz_mod(i,j),&
     l23z_mod(i,j),l23zz_mod(i,j),&
     !
     l11n1_mod(i,j),l21n2_mod(i,j),l11n12_mod(i,j),l21n22_mod(i,j),l112_mod(i,j),l122_mod(i,j),l132_mod(i,j),&
     l212_mod(i,j),l222_mod(i,j),l232_mod(i,j),&
     !
     f1_mod(i,j),f2_mod(i,j),f3_mod(i,j),f4_mod(i,j),f5_mod(i,j),f6_mod(i,j),f7_mod(i,j),f8_mod(i,j),f9_mod(i,j),f10_mod(i,j),&
     f11_mod(i,j),f12_mod(i,j),f13_mod(i,j),f14_mod(i,j),f15_mod(i,j),&
     f16_mod(i,j),f17_mod(i,j),f18_mod(i,j),f19_mod(i,j),f20_mod(i,j),&
     f21_mod(i,j),f22_mod(i,j),f23_mod(i,j),f24_mod(i,j),f25_mod(i,j),&
     f26_mod(i,j),f27_mod(i,j),f28_mod(i,j),f29_mod(i,j),f30_mod(i,j),&
     f31_mod(i,j),f32_mod(i,j),f33_mod(i,j),f34_mod(i,j),f35_mod(i,j),&
     f36_mod(i,j),f37_mod(i,j),f38_mod(i,j),f39_mod(i,j),f40_mod(i,j),&
     f41_mod(i,j),f42_mod(i,j),f43_mod(i,j),f44_mod(i,j),f45_mod(i,j),&
     f46_mod(i,j),f47_mod(i,j),f48_mod(i,j),f49_mod(i,j),f50_mod(i,j),&
     f51_mod(i,j),f52_mod(i,j),f53_mod(i,j),f54_mod(i,j),f55_mod(i,j),&
     f56_mod(i,j),f57_mod(i,j),f58_mod(i,j),f59_mod(i,j),f60_mod(i,j),&
     f61_mod(i,j),f62_mod(i,j),f63_mod(i,j),f64_mod(i,j),f65_mod(i,j),&
     f66_mod(i,j),f67_mod(i,j),f68_mod(i,j),f69_mod(i,j),f70_mod(i,j),&
     f71_mod(i,j),f72_mod(i,j),f73_mod(i,j),f74_mod(i,j),f75_mod(i,j),&
     f76_mod(i,j),f77_mod(i,j),f78_mod(i,j),f79_mod(i,j),f80_mod(i,j),&
     f81_mod(i,j),f82_mod(i,j),f83_mod(i,j),f84_mod(i,j),f85_mod(i,j),&
     f86_mod(i,j),f87_mod(i,j),f88_mod(i,j),f89_mod(i,j),f90_mod(i,j),&
     f91_mod(i,j),f92_mod(i,j),f93_mod(i,j),f94_mod(i,j),f95_mod(i,j),&
     f96_mod(i,j),f97_mod(i,j),f98_mod(i,j),f99_mod(i,j),f100_mod(i,j),&
     f101_mod(i,j),f102_mod(i,j),f103_mod(i,j),f104_mod(i,j),f105_mod(i,j),&
     f106_mod(i,j),f107_mod(i,j),f108_mod(i,j),f109_mod(i,j),f110_mod(i,j),&
     f111_mod(i,j),f112_mod(i,j),f113_mod(i,j),f114_mod(i,j),f115_mod(i,j),&
     f116_mod(i,j),f117_mod(i,j),f118_mod(i,j),f119_mod(i,j),&
     !
     ff1_mod(i,j),ff2_mod(i,j),ff3_mod(i,j),ff4_mod(i,j),ff5_mod(i,j),&
     ff6_mod(i,j),ff7_mod(i,j),ff8_mod(i,j),ff9_mod(i,j),ff10_mod(i,j),&
     ff11_mod(i,j),ff12_mod(i,j),ff13_mod(i,j),ff14_mod(i,j),ff15_mod(i,j),&
     ff16_mod(i,j),ff17_mod(i,j),ff18_mod(i,j),ff19_mod(i,j),ff20_mod(i,j),&
     ff21_mod(i,j),ff22_mod(i,j),ff23_mod(i,j),ff24_mod(i,j),ff25_mod(i,j),&
     ff26_mod(i,j),ff27_mod(i,j),ff28_mod(i,j),ff29_mod(i,j),ff30_mod(i,j),&
     ff31_mod(i,j),ff32_mod(i,j),ff33_mod(i,j),ff34_mod(i,j),ff35_mod(i,j))
 END DO
END DO

!******* fkcje a,d,kappa,Kstar,G1-8 oraz pochodne kappa,G1,G2 na siatce (x,y), bez pktow brzegowych, z uwzglednieniem zmod *******
DO i=2,Nx-1
 DO j=2,Ny-1
    CALL VarFunc(H1_mod(i,j),H1t_mod(i,j),H1tt_mod(i,j),H1tx_mod(i,j),H1ty_mod(i,j),H1tz_mod(i,j),&
     H1x_mod(i,j),H1xx_mod(i,j),H1xy_mod(i,j),H1xz_mod(i,j),H1y_mod(i,j),H1yy_mod(i,j),H1yz_mod(i,j),&
     H1z_mod(i,j),H1zz_mod(i,j),&
     H2_mod(i,j),H2t_mod(i,j),H2tt_mod(i,j),H2tx_mod(i,j),H2ty_mod(i,j),H2tz_mod(i,j),&
     H2x_mod(i,j),H2xx_mod(i,j),H2xy_mod(i,j),H2xz_mod(i,j),H2y_mod(i,j),H2yy_mod(i,j),H2yz_mod(i,j),&
     H2z_mod(i,j),H2zz_mod(i,j),H1H2_mod(i,j),&
     !
     l11_mod(i,j),l11t_mod(i,j),l11tt_mod(i,j),l11tx_mod(i,j),l11ty_mod(i,j),l11tz_mod(i,j),&
     l11x_mod(i,j),l11xx_mod(i,j),l11xy_mod(i,j),l11xz_mod(i,j),l11y_mod(i,j),l11yy_mod(i,j),l11yz_mod(i,j),&
     l11z_mod(i,j),l11zz_mod(i,j),&
     l12_mod(i,j),l12t_mod(i,j),l12tt_mod(i,j),l12tx_mod(i,j),l12ty_mod(i,j),l12tz_mod(i,j),&
     l12x_mod(i,j),l12xx_mod(i,j),l12xy_mod(i,j),l12xz_mod(i,j),l12y_mod(i,j),l12yy_mod(i,j),l12yz_mod(i,j),&
     l12z_mod(i,j),l12zz_mod(i,j),&
     l13_mod(i,j),l13t_mod(i,j),l13tt_mod(i,j),l13tx_mod(i,j),l13ty_mod(i,j),l13tz_mod(i,j),&
     l13x_mod(i,j),l13xx_mod(i,j),l13xy_mod(i,j),l13xz_mod(i,j),l13y_mod(i,j),l13yy_mod(i,j),l13yz_mod(i,j),&
     l13z_mod(i,j),l13zz_mod(i,j),&
     l21_mod(i,j),l21t_mod(i,j),l21tt_mod(i,j),l21tx_mod(i,j),l21ty_mod(i,j),l21tz_mod(i,j),&
     l21x_mod(i,j),l21xx_mod(i,j),l21xy_mod(i,j),l21xz_mod(i,j),l21y_mod(i,j),l21yy_mod(i,j),l21yz_mod(i,j),&
     l21z_mod(i,j),l21zz_mod(i,j),&
     l22_mod(i,j),l22t_mod(i,j),l22tt_mod(i,j),l22tx_mod(i,j),l22ty_mod(i,j),l22tz_mod(i,j),&
     l22x_mod(i,j),l22xx_mod(i,j),l22xy_mod(i,j),l22xz_mod(i,j),l22y_mod(i,j),l22yy_mod(i,j),l22yz_mod(i,j),&
     l22z_mod(i,j),l22zz_mod(i,j),&
     l23_mod(i,j),l23t_mod(i,j),l23tt_mod(i,j),l23tx_mod(i,j),l23ty_mod(i,j),l23tz_mod(i,j),&
     l23x_mod(i,j),l23xx_mod(i,j),l23xy_mod(i,j),l23xz_mod(i,j),l23y_mod(i,j),l23yy_mod(i,j),l23yz_mod(i,j),&
     l23z_mod(i,j),l23zz_mod(i,j),&
     !
     l11n1_mod(i,j),l21n2_mod(i,j),l11n12_mod(i,j),l21n22_mod(i,j),l112_mod(i,j),l122_mod(i,j),l132_mod(i,j),&
     l212_mod(i,j),l222_mod(i,j),l232_mod(i,j),&
     !
     f1_mod(i,j),f2_mod(i,j),f3_mod(i,j),f4_mod(i,j),f5_mod(i,j),f6_mod(i,j),f7_mod(i,j),f8_mod(i,j),f9_mod(i,j),f10_mod(i,j),&
     f11_mod(i,j),f12_mod(i,j),f13_mod(i,j),f14_mod(i,j),f15_mod(i,j),&
     f16_mod(i,j),f17_mod(i,j),f18_mod(i,j),f19_mod(i,j),f20_mod(i,j),&
     f21_mod(i,j),f22_mod(i,j),f23_mod(i,j),f24_mod(i,j),f25_mod(i,j),&
     f26_mod(i,j),f27_mod(i,j),f28_mod(i,j),f29_mod(i,j),f30_mod(i,j),&
     f31_mod(i,j),f32_mod(i,j),f33_mod(i,j),f34_mod(i,j),f35_mod(i,j),&
     f36_mod(i,j),f37_mod(i,j),f38_mod(i,j),f39_mod(i,j),f40_mod(i,j),&
     f41_mod(i,j),f42_mod(i,j),f43_mod(i,j),f44_mod(i,j),f45_mod(i,j),&
     f46_mod(i,j),f47_mod(i,j),f48_mod(i,j),f49_mod(i,j),f50_mod(i,j),&
     f51_mod(i,j),f52_mod(i,j),f53_mod(i,j),f54_mod(i,j),f55_mod(i,j),&
     f56_mod(i,j),f57_mod(i,j),f58_mod(i,j),f59_mod(i,j),f60_mod(i,j),&
     f61_mod(i,j),f62_mod(i,j),f63_mod(i,j),f64_mod(i,j),f65_mod(i,j),&
     f66_mod(i,j),f67_mod(i,j),f68_mod(i,j),f69_mod(i,j),f70_mod(i,j),&
     f71_mod(i,j),f72_mod(i,j),f73_mod(i,j),f74_mod(i,j),f75_mod(i,j),&
     f76_mod(i,j),f77_mod(i,j),f78_mod(i,j),f79_mod(i,j),f80_mod(i,j),&
     f81_mod(i,j),f82_mod(i,j),f83_mod(i,j),f84_mod(i,j),f85_mod(i,j),&
     f86_mod(i,j),f87_mod(i,j),f88_mod(i,j),f89_mod(i,j),f90_mod(i,j),&
     f91_mod(i,j),f92_mod(i,j),f93_mod(i,j),f94_mod(i,j),f95_mod(i,j),&
     f96_mod(i,j),f97_mod(i,j),f98_mod(i,j),f99_mod(i,j),f100_mod(i,j),&
     f101_mod(i,j),f102_mod(i,j),f103_mod(i,j),f104_mod(i,j),f105_mod(i,j),&
     f106_mod(i,j),f107_mod(i,j),f108_mod(i,j),f109_mod(i,j),f110_mod(i,j),&
     f111_mod(i,j),f112_mod(i,j),f113_mod(i,j),f114_mod(i,j),f115_mod(i,j),&
     f116_mod(i,j),f117_mod(i,j),f118_mod(i,j),f119_mod(i,j),&
     !
     ff1_mod(i,j),ff2_mod(i,j),ff3_mod(i,j),ff4_mod(i,j),ff5_mod(i,j),&
     ff6_mod(i,j),ff7_mod(i,j),ff8_mod(i,j),ff9_mod(i,j),ff10_mod(i,j),&
     ff11_mod(i,j),ff12_mod(i,j),ff13_mod(i,j),ff14_mod(i,j),ff15_mod(i,j),&
     ff16_mod(i,j),ff17_mod(i,j),ff18_mod(i,j),ff19_mod(i,j),ff20_mod(i,j),&
     ff21_mod(i,j),ff22_mod(i,j),ff23_mod(i,j),ff24_mod(i,j),ff25_mod(i,j),&
     ff26_mod(i,j),ff27_mod(i,j),ff28_mod(i,j),ff29_mod(i,j),ff30_mod(i,j),&
     ff31_mod(i,j),ff32_mod(i,j),ff33_mod(i,j),ff34_mod(i,j),ff35_mod(i,j),&
     !OUT:
     G1_mod(i,j),G2_mod(i,j),G3_mod(i,j),G4_mod(i,j),G5_mod(i,j),G6_mod(i,j),G7_mod(i,j),G8_mod(i,j),&
     dxG1_mod(i,j),dyG1_mod(i,j),dxG2_mod(i,j),dyG2_mod(i,j),&
     a_mod(i,j),d_mod(i,j),kappa_mod(i,j),Kstar_mod(i,j),dxkappa_mod(i,j),dykappa_mod(i,j))
    !
    CALL FunctionsEvAll(H1_mod(i,j),H1t_mod(i,j),H1tt_mod(i,j),H1tx_mod(i,j),H1ty_mod(i,j),H1tz_mod(i,j),&
     H1x_mod(i,j),H1xx_mod(i,j),H1xy_mod(i,j),H1xz_mod(i,j),H1y_mod(i,j),H1yy_mod(i,j),H1yz_mod(i,j),&
     H1z_mod(i,j),H1zz_mod(i,j),&
     H2_mod(i,j),H2t_mod(i,j),H2tt_mod(i,j),H2tx_mod(i,j),H2ty_mod(i,j),H2tz_mod(i,j),&
     H2x_mod(i,j),H2xx_mod(i,j),H2xy_mod(i,j),H2xz_mod(i,j),H2y_mod(i,j),H2yy_mod(i,j),H2yz_mod(i,j),&
     H2z_mod(i,j),H2zz_mod(i,j),H1H2_mod(i,j),&
     !
     l11_mod(i,j),l11t_mod(i,j),l11tt_mod(i,j),l11tx_mod(i,j),l11ty_mod(i,j),l11tz_mod(i,j),&
     l11x_mod(i,j),l11xx_mod(i,j),l11xy_mod(i,j),l11xz_mod(i,j),l11y_mod(i,j),l11yy_mod(i,j),l11yz_mod(i,j),&
     l11z_mod(i,j),l11zz_mod(i,j),&
     l12_mod(i,j),l12t_mod(i,j),l12tt_mod(i,j),l12tx_mod(i,j),l12ty_mod(i,j),l12tz_mod(i,j),&
     l12x_mod(i,j),l12xx_mod(i,j),l12xy_mod(i,j),l12xz_mod(i,j),l12y_mod(i,j),l12yy_mod(i,j),l12yz_mod(i,j),&
     l12z_mod(i,j),l12zz_mod(i,j),&
     l13_mod(i,j),l13t_mod(i,j),l13tt_mod(i,j),l13tx_mod(i,j),l13ty_mod(i,j),l13tz_mod(i,j),&
     l13x_mod(i,j),l13xx_mod(i,j),l13xy_mod(i,j),l13xz_mod(i,j),l13y_mod(i,j),l13yy_mod(i,j),l13yz_mod(i,j),&
     l13z_mod(i,j),l13zz_mod(i,j),&
     l21_mod(i,j),l21t_mod(i,j),l21tt_mod(i,j),l21tx_mod(i,j),l21ty_mod(i,j),l21tz_mod(i,j),&
     l21x_mod(i,j),l21xx_mod(i,j),l21xy_mod(i,j),l21xz_mod(i,j),l21y_mod(i,j),l21yy_mod(i,j),l21yz_mod(i,j),&
     l21z_mod(i,j),l21zz_mod(i,j),&
     l22_mod(i,j),l22t_mod(i,j),l22tt_mod(i,j),l22tx_mod(i,j),l22ty_mod(i,j),l22tz_mod(i,j),&
     l22x_mod(i,j),l22xx_mod(i,j),l22xy_mod(i,j),l22xz_mod(i,j),l22y_mod(i,j),l22yy_mod(i,j),l22yz_mod(i,j),&
     l22z_mod(i,j),l22zz_mod(i,j),&
     l23_mod(i,j),l23t_mod(i,j),l23tt_mod(i,j),l23tx_mod(i,j),l23ty_mod(i,j),l23tz_mod(i,j),&
     l23x_mod(i,j),l23xx_mod(i,j),l23xy_mod(i,j),l23xz_mod(i,j),l23y_mod(i,j),l23yy_mod(i,j),l23yz_mod(i,j),&
     l23z_mod(i,j),l23zz_mod(i,j),&
     !
     l11n1_mod(i,j),l21n2_mod(i,j),l11n12_mod(i,j),l21n22_mod(i,j),l112_mod(i,j),l122_mod(i,j),l132_mod(i,j),&
     l212_mod(i,j),l222_mod(i,j),l232_mod(i,j),&
     !
     f1_mod(i,j),f2_mod(i,j),f3_mod(i,j),f4_mod(i,j),f5_mod(i,j),f6_mod(i,j),f7_mod(i,j),f8_mod(i,j),f9_mod(i,j),f10_mod(i,j),&
     f11_mod(i,j),f12_mod(i,j),f13_mod(i,j),f14_mod(i,j),f15_mod(i,j),&
     f16_mod(i,j),f17_mod(i,j),f18_mod(i,j),f19_mod(i,j),f20_mod(i,j),&
     f21_mod(i,j),f22_mod(i,j),f23_mod(i,j),f24_mod(i,j),f25_mod(i,j),&
     f26_mod(i,j),f27_mod(i,j),f28_mod(i,j),f29_mod(i,j),f30_mod(i,j),&
     f31_mod(i,j),f32_mod(i,j),f33_mod(i,j),f34_mod(i,j),f35_mod(i,j),&
     f36_mod(i,j),f37_mod(i,j),f38_mod(i,j),f39_mod(i,j),f40_mod(i,j),&
     f41_mod(i,j),f42_mod(i,j),f43_mod(i,j),f44_mod(i,j),f45_mod(i,j),&
     f46_mod(i,j),f47_mod(i,j),f48_mod(i,j),f49_mod(i,j),f50_mod(i,j),&
     f51_mod(i,j),f52_mod(i,j),f53_mod(i,j),f54_mod(i,j),f55_mod(i,j),&
     f56_mod(i,j),f57_mod(i,j),f58_mod(i,j),f59_mod(i,j),f60_mod(i,j),&
     f61_mod(i,j),f62_mod(i,j),f63_mod(i,j),f64_mod(i,j),f65_mod(i,j),&
     f66_mod(i,j),f67_mod(i,j),f68_mod(i,j),f69_mod(i,j),f70_mod(i,j),&
     f71_mod(i,j),f72_mod(i,j),f73_mod(i,j),f74_mod(i,j),f75_mod(i,j),&
     f76_mod(i,j),f77_mod(i,j),f78_mod(i,j),f79_mod(i,j),f80_mod(i,j),&
     f81_mod(i,j),f82_mod(i,j),f83_mod(i,j),f84_mod(i,j),f85_mod(i,j),&
     f86_mod(i,j),f87_mod(i,j),f88_mod(i,j),f89_mod(i,j),f90_mod(i,j),&
     f91_mod(i,j),f92_mod(i,j),f93_mod(i,j),f94_mod(i,j),f95_mod(i,j),&
     f96_mod(i,j),f97_mod(i,j),f98_mod(i,j),f99_mod(i,j),f100_mod(i,j),&
     f101_mod(i,j),f102_mod(i,j),f103_mod(i,j),f104_mod(i,j),f105_mod(i,j),&
     f106_mod(i,j),f107_mod(i,j),f108_mod(i,j),f109_mod(i,j),f110_mod(i,j),&
     f111_mod(i,j),f112_mod(i,j),f113_mod(i,j),f114_mod(i,j),f115_mod(i,j),&
     f116_mod(i,j),f117_mod(i,j),f118_mod(i,j),f119_mod(i,j),&
     !
     ff1_mod(i,j),ff2_mod(i,j),ff3_mod(i,j),ff4_mod(i,j),ff5_mod(i,j),&
     ff6_mod(i,j),ff7_mod(i,j),ff8_mod(i,j),ff9_mod(i,j),ff10_mod(i,j),&
     ff11_mod(i,j),ff12_mod(i,j),ff13_mod(i,j),ff14_mod(i,j),ff15_mod(i,j),&
     ff16_mod(i,j),ff17_mod(i,j),ff18_mod(i,j),ff19_mod(i,j),ff20_mod(i,j),&
     ff21_mod(i,j),ff22_mod(i,j),ff23_mod(i,j),ff24_mod(i,j),ff25_mod(i,j),&
     ff26_mod(i,j),ff27_mod(i,j),ff28_mod(i,j),ff29_mod(i,j),ff30_mod(i,j),&
     ff31_mod(i,j),ff32_mod(i,j),ff33_mod(i,j),ff34_mod(i,j),ff35_mod(i,j),&
     !OUT:
     Ak1_mod(i,j),Ak2_mod(i,j),ANh_mod(i,j),AKK_mod(i,j))
    !
    CALL AnalDerivs(H1_mod(i,j),H1t_mod(i,j),H1tt_mod(i,j),H1tx_mod(i,j),H1ty_mod(i,j),H1tz_mod(i,j),&
     H1x_mod(i,j),H1xx_mod(i,j),H1xy_mod(i,j),H1xz_mod(i,j),H1y_mod(i,j),H1yy_mod(i,j),H1yz_mod(i,j),&
     H1z_mod(i,j),H1zz_mod(i,j),&
     H2_mod(i,j),H2t_mod(i,j),H2tt_mod(i,j),H2tx_mod(i,j),H2ty_mod(i,j),H2tz_mod(i,j),&
     H2x_mod(i,j),H2xx_mod(i,j),H2xy_mod(i,j),H2xz_mod(i,j),H2y_mod(i,j),H2yy_mod(i,j),H2yz_mod(i,j),&
     H2z_mod(i,j),H2zz_mod(i,j),H1H2_mod(i,j),&
     !
     l11_mod(i,j),l11t_mod(i,j),l11tt_mod(i,j),l11tx_mod(i,j),l11ty_mod(i,j),l11tz_mod(i,j),&
     l11x_mod(i,j),l11xx_mod(i,j),l11xy_mod(i,j),l11xz_mod(i,j),l11y_mod(i,j),l11yy_mod(i,j),l11yz_mod(i,j),&
     l11z_mod(i,j),l11zz_mod(i,j),&
     l12_mod(i,j),l12t_mod(i,j),l12tt_mod(i,j),l12tx_mod(i,j),l12ty_mod(i,j),l12tz_mod(i,j),&
     l12x_mod(i,j),l12xx_mod(i,j),l12xy_mod(i,j),l12xz_mod(i,j),l12y_mod(i,j),l12yy_mod(i,j),l12yz_mod(i,j),&
     l12z_mod(i,j),l12zz_mod(i,j),&
     l13_mod(i,j),l13t_mod(i,j),l13tt_mod(i,j),l13tx_mod(i,j),l13ty_mod(i,j),l13tz_mod(i,j),&
     l13x_mod(i,j),l13xx_mod(i,j),l13xy_mod(i,j),l13xz_mod(i,j),l13y_mod(i,j),l13yy_mod(i,j),l13yz_mod(i,j),&
     l13z_mod(i,j),l13zz_mod(i,j),&
     l21_mod(i,j),l21t_mod(i,j),l21tt_mod(i,j),l21tx_mod(i,j),l21ty_mod(i,j),l21tz_mod(i,j),&
     l21x_mod(i,j),l21xx_mod(i,j),l21xy_mod(i,j),l21xz_mod(i,j),l21y_mod(i,j),l21yy_mod(i,j),l21yz_mod(i,j),&
     l21z_mod(i,j),l21zz_mod(i,j),&
     l22_mod(i,j),l22t_mod(i,j),l22tt_mod(i,j),l22tx_mod(i,j),l22ty_mod(i,j),l22tz_mod(i,j),&
     l22x_mod(i,j),l22xx_mod(i,j),l22xy_mod(i,j),l22xz_mod(i,j),l22y_mod(i,j),l22yy_mod(i,j),l22yz_mod(i,j),&
     l22z_mod(i,j),l22zz_mod(i,j),&
     l23_mod(i,j),l23t_mod(i,j),l23tt_mod(i,j),l23tx_mod(i,j),l23ty_mod(i,j),l23tz_mod(i,j),&
     l23x_mod(i,j),l23xx_mod(i,j),l23xy_mod(i,j),l23xz_mod(i,j),l23y_mod(i,j),l23yy_mod(i,j),l23yz_mod(i,j),&
     l23z_mod(i,j),l23zz_mod(i,j),&
     !
     l11n1_mod(i,j),l21n2_mod(i,j),l11n12_mod(i,j),l21n22_mod(i,j),l112_mod(i,j),l122_mod(i,j),l132_mod(i,j),&
     l212_mod(i,j),l222_mod(i,j),l232_mod(i,j),&
     !
     f1_mod(i,j),f2_mod(i,j),f3_mod(i,j),f4_mod(i,j),f5_mod(i,j),f6_mod(i,j),f7_mod(i,j),f8_mod(i,j),f9_mod(i,j),f10_mod(i,j),&
     f11_mod(i,j),f12_mod(i,j),f13_mod(i,j),f14_mod(i,j),f15_mod(i,j),&
     f16_mod(i,j),f17_mod(i,j),f18_mod(i,j),f19_mod(i,j),f20_mod(i,j),&
     f21_mod(i,j),f22_mod(i,j),f23_mod(i,j),f24_mod(i,j),f25_mod(i,j),&
     f26_mod(i,j),f27_mod(i,j),f28_mod(i,j),f29_mod(i,j),f30_mod(i,j),&
     f31_mod(i,j),f32_mod(i,j),f33_mod(i,j),f34_mod(i,j),f35_mod(i,j),&
     f36_mod(i,j),f37_mod(i,j),f38_mod(i,j),f39_mod(i,j),f40_mod(i,j),&
     f41_mod(i,j),f42_mod(i,j),f43_mod(i,j),f44_mod(i,j),f45_mod(i,j),&
     f46_mod(i,j),f47_mod(i,j),f48_mod(i,j),f49_mod(i,j),f50_mod(i,j),&
     f51_mod(i,j),f52_mod(i,j),f53_mod(i,j),f54_mod(i,j),f55_mod(i,j),&
     f56_mod(i,j),f57_mod(i,j),f58_mod(i,j),f59_mod(i,j),f60_mod(i,j),&
     f61_mod(i,j),f62_mod(i,j),f63_mod(i,j),f64_mod(i,j),f65_mod(i,j),&
     f66_mod(i,j),f67_mod(i,j),f68_mod(i,j),f69_mod(i,j),f70_mod(i,j),&
     f71_mod(i,j),f72_mod(i,j),f73_mod(i,j),f74_mod(i,j),f75_mod(i,j),&
     f76_mod(i,j),f77_mod(i,j),f78_mod(i,j),f79_mod(i,j),f80_mod(i,j),&
     f81_mod(i,j),f82_mod(i,j),f83_mod(i,j),f84_mod(i,j),f85_mod(i,j),&
     f86_mod(i,j),f87_mod(i,j),f88_mod(i,j),f89_mod(i,j),f90_mod(i,j),&
     f91_mod(i,j),f92_mod(i,j),f93_mod(i,j),f94_mod(i,j),f95_mod(i,j),&
     f96_mod(i,j),f97_mod(i,j),f98_mod(i,j),f99_mod(i,j),f100_mod(i,j),&
     f101_mod(i,j),f102_mod(i,j),f103_mod(i,j),f104_mod(i,j),f105_mod(i,j),&
     f106_mod(i,j),f107_mod(i,j),f108_mod(i,j),f109_mod(i,j),f110_mod(i,j),&
     f111_mod(i,j),f112_mod(i,j),f113_mod(i,j),f114_mod(i,j),f115_mod(i,j),&
     f116_mod(i,j),f117_mod(i,j),f118_mod(i,j),f119_mod(i,j),&
     !
     ff1_mod(i,j),ff2_mod(i,j),ff3_mod(i,j),ff4_mod(i,j),ff5_mod(i,j),&
     ff6_mod(i,j),ff7_mod(i,j),ff8_mod(i,j),ff9_mod(i,j),ff10_mod(i,j),&
     ff11_mod(i,j),ff12_mod(i,j),ff13_mod(i,j),ff14_mod(i,j),ff15_mod(i,j),&
     ff16_mod(i,j),ff17_mod(i,j),ff18_mod(i,j),ff19_mod(i,j),ff20_mod(i,j),&
     ff21_mod(i,j),ff22_mod(i,j),ff23_mod(i,j),ff24_mod(i,j),ff25_mod(i,j),&
     ff26_mod(i,j),ff27_mod(i,j),ff28_mod(i,j),ff29_mod(i,j),ff30_mod(i,j),&
     ff31_mod(i,j),ff32_mod(i,j),ff33_mod(i,j),ff34_mod(i,j),ff35_mod(i,j),&
     !OUT:
     AdxNh_mod(i,j),AdyNh_mod(i,j),AdxKK_mod(i,j),AdyKK_mod(i,j),Adxk1_mod(i,j),Adyk1_mod(i,j),Adxk2_mod(i,j),Adyk2_mod(i,j),&
     AdxxNh_mod(i,j),AdyyNh_mod(i,j),AdxyNh_mod(i,j))
 END DO
END DO

!******* fkcje k1mod,k2mod,Nhmod,KKmod na siatce (x,y) *******
k1mod=k1+0.5D0*dz*k1k1 !k1mod(Nx,Ny)
k2mod=k2+0.5D0*dz*k1k2 !k2mod(Nx,Ny)
Nhmod=Nh+0.5D0*dz*k1Nh !Nhmod(Nx,Ny)
KKmod=KK+0.5D0*dz*k1KK !KKmod(Nx,Ny)
!* war brzeg
i=1
DO j=1,Ny
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO
i=Nx
DO j=1,Ny
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO
!
j=1
DO i=2,Nx-1
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO
j=Ny
DO i=2,Nx-1
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO

!******* pochodne fkcji k1mod,k2mod,Nhmod,KKmod na siatce (x,y) *******
CALL Pochodne(k1mod,k2mod,Nhmod,KKmod,&
 !OUT:
 dxNhmod,dyNhmod,dxxNhmod,dyyNhmod,dxyNhmod,dxKKmod,dyKKmod,dxk1mod,dyk1mod,dxk2mod,dyk2mod)

!******* RK4-k2 na siatce (x,y) *******
CALL k1234(H1_mod,H1t_mod,H1tt_mod,H1tx_mod,H1ty_mod,H1tz_mod,H1x_mod,H1xx_mod,H1xy_mod,H1xz_mod,H1y_mod,H1yy_mod,H1yz_mod,&
 H1z_mod,H1zz_mod,&
 H2_mod,H2t_mod,H2tt_mod,H2tx_mod,H2ty_mod,H2tz_mod,H2x_mod,H2xx_mod,H2xy_mod,H2xz_mod,H2y_mod,H2yy_mod,H2yz_mod,&
 H2z_mod,H2zz_mod,H1H2_mod,&
 !
 l11_mod,l11t_mod,l11tt_mod,l11tx_mod,l11ty_mod,l11tz_mod,l11x_mod,l11xx_mod,l11xy_mod,l11xz_mod,&
 l11y_mod,l11yy_mod,l11yz_mod,l11z_mod,l11zz_mod,&
 l12_mod,l12t_mod,l12tt_mod,l12tx_mod,l12ty_mod,l12tz_mod,l12x_mod,l12xx_mod,l12xy_mod,l12xz_mod,&
 l12y_mod,l12yy_mod,l12yz_mod,l12z_mod,l12zz_mod,&
 l13_mod,l13t_mod,l13tt_mod,l13tx_mod,l13ty_mod,l13tz_mod,l13x_mod,l13xx_mod,l13xy_mod,l13xz_mod,&
 l13y_mod,l13yy_mod,l13yz_mod,l13z_mod,l13zz_mod,&
 l21_mod,l21t_mod,l21tt_mod,l21tx_mod,l21ty_mod,l21tz_mod,l21x_mod,l21xx_mod,l21xy_mod,l21xz_mod,&
 l21y_mod,l21yy_mod,l21yz_mod,l21z_mod,l21zz_mod,&
 l22_mod,l22t_mod,l22tt_mod,l22tx_mod,l22ty_mod,l22tz_mod,l22x_mod,l22xx_mod,l22xy_mod,l22xz_mod,&
 l22y_mod,l22yy_mod,l22yz_mod,l22z_mod,l22zz_mod,&
 l23_mod,l23t_mod,l23tt_mod,l23tx_mod,l23ty_mod,l23tz_mod,l23x_mod,l23xx_mod,l23xy_mod,l23xz_mod,&
 l23y_mod,l23yy_mod,l23yz_mod,l23z_mod,l23zz_mod,&
 !
 l11n1_mod,l21n2_mod,l11n12_mod,l21n22_mod,l112_mod,l122_mod,l132_mod,l212_mod,l222_mod,l232_mod,&
 !
 f1_mod,f2_mod,f3_mod,f4_mod,f5_mod,f6_mod,f7_mod,f8_mod,f9_mod,f10_mod,f11_mod,f12_mod,f13_mod,f14_mod,f15_mod,&
 f16_mod,f17_mod,f18_mod,f19_mod,f20_mod,f21_mod,f22_mod,f23_mod,f24_mod,f25_mod,f26_mod,f27_mod,f28_mod,f29_mod,f30_mod,&
 f31_mod,f32_mod,f33_mod,f34_mod,f35_mod,f36_mod,f37_mod,f38_mod,f39_mod,f40_mod,f41_mod,f42_mod,f43_mod,f44_mod,f45_mod,&
 f46_mod,f47_mod,f48_mod,f49_mod,f50_mod,f51_mod,f52_mod,f53_mod,f54_mod,f55_mod,f56_mod,f57_mod,f58_mod,f59_mod,f60_mod,&
 f61_mod,f62_mod,f63_mod,f64_mod,f65_mod,f66_mod,f67_mod,f68_mod,f69_mod,f70_mod,f71_mod,f72_mod,f73_mod,f74_mod,f75_mod,&
 f76_mod,f77_mod,f78_mod,f79_mod,f80_mod,f81_mod,f82_mod,f83_mod,f84_mod,f85_mod,f86_mod,f87_mod,f88_mod,f89_mod,f90_mod,&
 f91_mod,f92_mod,f93_mod,f94_mod,f95_mod,f96_mod,f97_mod,f98_mod,f99_mod,f100_mod,f101_mod,f102_mod,f103_mod,f104_mod,f105_mod,&
 f106_mod,f107_mod,f108_mod,f109_mod,f110_mod,f111_mod,f112_mod,f113_mod,f114_mod,f115_mod,f116_mod,f117_mod,f118_mod,f119_mod,&
 !
 ff1_mod,ff2_mod,ff3_mod,ff4_mod,ff5_mod,ff6_mod,ff7_mod,ff8_mod,ff9_mod,ff10_mod,ff11_mod,ff12_mod,ff13_mod,ff14_mod,ff15_mod,&
 ff16_mod,ff17_mod,ff18_mod,ff19_mod,ff20_mod,ff21_mod,ff22_mod,ff23_mod,ff24_mod,ff25_mod,&
 ff26_mod,ff27_mod,ff28_mod,ff29_mod,ff30_mod,ff31_mod,ff32_mod,ff33_mod,ff34_mod,ff35_mod,&
 !
 G1_mod,G2_mod,G3_mod,G4_mod,G5_mod,G6_mod,G7_mod,G8_mod,dxG1_mod,dyG1_mod,dxG2_mod,dyG2_mod,&
 a_mod,d_mod,kappa_mod,Kstar_mod,k1mod,k2mod,Nhmod,KKmod,dxNhmod,dyNhmod,dxKKmod,dyKKmod,dxkappa_mod,dykappa_mod,&
 dxk1mod,dyk1mod,dxk2mod,dyk2mod,dxxNhmod,dyyNhmod,dxyNhmod,&
 Ak1_mod,Ak2_mod,ANh_mod,AKK_mod,AdxNh_mod,AdyNh_mod,AdxKK_mod,AdyKK_mod,Adxk1_mod,Adyk1_mod,Adxk2_mod,Adyk2_mod,&
 AdxxNh_mod,AdyyNh_mod,AdxyNh_mod,&
 !OUT:
 k2KK,k2k1,k2k2,k2Nh)


!* do RK4-k3 *****************************************************************************************************************
!******* fkcje pomocnicze H i l na siatce (x,y), z uwzglednieniem zmod *******
!j.w.
!******* fkcje a,d,kappa,Kstar,G1-8 oraz pochodne kappa,G1,G2 na siatce (x,y), bez pktow brzegowych, z uwzglednieniem zmod  *******
!j.w.

!******* fkcje k1mod,k2mod,Nhmod,KKmod na siatce (x,y) *******
k1mod=k1+0.5D0*dz*k2k1 !k1mod(Nx,Ny)
k2mod=k2+0.5D0*dz*k2k2 !k2mod(Nx,Ny)
Nhmod=Nh+0.5D0*dz*k2Nh !Nhmod(Nx,Ny)
KKmod=KK+0.5D0*dz*k2KK !KKmod(Nx,Ny)
!* war brzeg
i=1
DO j=1,Ny
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO
i=Nx
DO j=1,Ny
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO
!
j=1
DO i=2,Nx-1
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO
j=Ny
DO i=2,Nx-1
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO

!******* pochodne fkcji k1mod,k2mod,Nhmod,KKmod na siatce (x,y) *******
CALL Pochodne(k1mod,k2mod,Nhmod,KKmod,&
 !OUT:
 dxNhmod,dyNhmod,dxxNhmod,dyyNhmod,dxyNhmod,dxKKmod,dyKKmod,dxk1mod,dyk1mod,dxk2mod,dyk2mod)

!******* RK4-k3 na siatce (x,y) *******
CALL k1234(H1_mod,H1t_mod,H1tt_mod,H1tx_mod,H1ty_mod,H1tz_mod,H1x_mod,H1xx_mod,H1xy_mod,H1xz_mod,H1y_mod,H1yy_mod,H1yz_mod,&
 H1z_mod,H1zz_mod,&
 H2_mod,H2t_mod,H2tt_mod,H2tx_mod,H2ty_mod,H2tz_mod,H2x_mod,H2xx_mod,H2xy_mod,H2xz_mod,H2y_mod,H2yy_mod,H2yz_mod,&
 H2z_mod,H2zz_mod,H1H2_mod,&
 !
 l11_mod,l11t_mod,l11tt_mod,l11tx_mod,l11ty_mod,l11tz_mod,l11x_mod,l11xx_mod,l11xy_mod,l11xz_mod,&
 l11y_mod,l11yy_mod,l11yz_mod,l11z_mod,l11zz_mod,&
 l12_mod,l12t_mod,l12tt_mod,l12tx_mod,l12ty_mod,l12tz_mod,l12x_mod,l12xx_mod,l12xy_mod,l12xz_mod,&
 l12y_mod,l12yy_mod,l12yz_mod,l12z_mod,l12zz_mod,&
 l13_mod,l13t_mod,l13tt_mod,l13tx_mod,l13ty_mod,l13tz_mod,l13x_mod,l13xx_mod,l13xy_mod,l13xz_mod,&
 l13y_mod,l13yy_mod,l13yz_mod,l13z_mod,l13zz_mod,&
 l21_mod,l21t_mod,l21tt_mod,l21tx_mod,l21ty_mod,l21tz_mod,l21x_mod,l21xx_mod,l21xy_mod,l21xz_mod,&
 l21y_mod,l21yy_mod,l21yz_mod,l21z_mod,l21zz_mod,&
 l22_mod,l22t_mod,l22tt_mod,l22tx_mod,l22ty_mod,l22tz_mod,l22x_mod,l22xx_mod,l22xy_mod,l22xz_mod,&
 l22y_mod,l22yy_mod,l22yz_mod,l22z_mod,l22zz_mod,&
 l23_mod,l23t_mod,l23tt_mod,l23tx_mod,l23ty_mod,l23tz_mod,l23x_mod,l23xx_mod,l23xy_mod,l23xz_mod,&
 l23y_mod,l23yy_mod,l23yz_mod,l23z_mod,l23zz_mod,&
 !
 l11n1_mod,l21n2_mod,l11n12_mod,l21n22_mod,l112_mod,l122_mod,l132_mod,l212_mod,l222_mod,l232_mod,&
 !
 f1_mod,f2_mod,f3_mod,f4_mod,f5_mod,f6_mod,f7_mod,f8_mod,f9_mod,f10_mod,f11_mod,f12_mod,f13_mod,f14_mod,f15_mod,&
 f16_mod,f17_mod,f18_mod,f19_mod,f20_mod,f21_mod,f22_mod,f23_mod,f24_mod,f25_mod,f26_mod,f27_mod,f28_mod,f29_mod,f30_mod,&
 f31_mod,f32_mod,f33_mod,f34_mod,f35_mod,f36_mod,f37_mod,f38_mod,f39_mod,f40_mod,f41_mod,f42_mod,f43_mod,f44_mod,f45_mod,&
 f46_mod,f47_mod,f48_mod,f49_mod,f50_mod,f51_mod,f52_mod,f53_mod,f54_mod,f55_mod,f56_mod,f57_mod,f58_mod,f59_mod,f60_mod,&
 f61_mod,f62_mod,f63_mod,f64_mod,f65_mod,f66_mod,f67_mod,f68_mod,f69_mod,f70_mod,f71_mod,f72_mod,f73_mod,f74_mod,f75_mod,&
 f76_mod,f77_mod,f78_mod,f79_mod,f80_mod,f81_mod,f82_mod,f83_mod,f84_mod,f85_mod,f86_mod,f87_mod,f88_mod,f89_mod,f90_mod,&
 f91_mod,f92_mod,f93_mod,f94_mod,f95_mod,f96_mod,f97_mod,f98_mod,f99_mod,f100_mod,f101_mod,f102_mod,f103_mod,f104_mod,f105_mod,&
 f106_mod,f107_mod,f108_mod,f109_mod,f110_mod,f111_mod,f112_mod,f113_mod,f114_mod,f115_mod,f116_mod,f117_mod,f118_mod,f119_mod,&
 !
 ff1_mod,ff2_mod,ff3_mod,ff4_mod,ff5_mod,ff6_mod,ff7_mod,ff8_mod,ff9_mod,ff10_mod,ff11_mod,ff12_mod,ff13_mod,ff14_mod,ff15_mod,&
 ff16_mod,ff17_mod,ff18_mod,ff19_mod,ff20_mod,ff21_mod,ff22_mod,ff23_mod,ff24_mod,ff25_mod,&
 ff26_mod,ff27_mod,ff28_mod,ff29_mod,ff30_mod,ff31_mod,ff32_mod,ff33_mod,ff34_mod,ff35_mod,&
 !
 G1_mod,G2_mod,G3_mod,G4_mod,G5_mod,G6_mod,G7_mod,G8_mod,dxG1_mod,dyG1_mod,dxG2_mod,dyG2_mod,&
 a_mod,d_mod,kappa_mod,Kstar_mod,k1mod,k2mod,Nhmod,KKmod,dxNhmod,dyNhmod,dxKKmod,dyKKmod,dxkappa_mod,dykappa_mod,&
 dxk1mod,dyk1mod,dxk2mod,dyk2mod,dxxNhmod,dyyNhmod,dxyNhmod,&
 Ak1_mod,Ak2_mod,ANh_mod,AKK_mod,AdxNh_mod,AdyNh_mod,AdxKK_mod,AdyKK_mod,Adxk1_mod,Adyk1_mod,Adxk2_mod,Adyk2_mod,&
 AdxxNh_mod,AdyyNh_mod,AdxyNh_mod,&
 !OUT:
 k3KK,k3k1,k3k2,k3Nh)


!* do RK4-k4 *****************************************************************************************************************
!******* fkcje pomocnicze H i l na siatce (x,y), z uwzglednieniem zmod *******
!przekazane z programu glownego (bez pz)
!******* fkcje a,d,kappa,Kstar,G1-8 oraz pochodne kappa,G1,G2 na siatce (x,y), bez pktow brzegowych, z uwzglednieniem zmod  *******
!przekazane z programu glownego (bez pz)

!******* fkcje k1mod,k2mod,Nhmod,KKmod na siatce (x,y) *******
k1mod=k1+dz*k3k1 !k1mod(Nx,Ny)
k2mod=k2+dz*k3k2 !k2mod(Nx,Ny)
Nhmod=Nh+dz*k3Nh !Nhmod(Nx,Ny)
KKmod=KK+dz*k3KK !KKmod(Nx,Ny)
!* war brzeg
i=1
DO j=1,Ny
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO
i=Nx
DO j=1,Ny
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO
!
j=1
DO i=2,Nx-1
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO
j=Ny
DO i=2,Nx-1
   k1mod(i,j)=0.D0
   k2mod(i,j)=0.D0
   Nhmod(i,j)=0.D0
   KKmod(i,j)=0.D0
END DO

!******* pochodne fkcji k1mod,k2mod,Nhmod,KKmod na siatce (x,y) *******
CALL Pochodne(k1mod,k2mod,Nhmod,KKmod,&
 !OUT:
 dxNhmod,dyNhmod,dxxNhmod,dyyNhmod,dxyNhmod,dxKKmod,dyKKmod,dxk1mod,dyk1mod,dxk2mod,dyk2mod)

!******* RK4-k4 na siatce (x,y) *******
CALL k1234(H1,H1t,H1tt,H1tx,H1ty,H1tz,H1x,H1xx,H1xy,H1xz,H1y,H1yy,H1yz,H1z,H1zz,&
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
 G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,&
 k1mod,k2mod,Nhmod,KKmod,dxNhmod,dyNhmod,dxKKmod,dyKKmod,dxkappa,dykappa,&
 dxk1mod,dyk1mod,dxk2mod,dyk2mod,dxxNhmod,dyyNhmod,dxyNhmod,&
 Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh,&
 !OUT:
 k4KK,k4k1,k4k2,k4Nh)


!* RK4 ***********************************************************************************************************************
dzRK4KK=(k1KK+2.D0*k2KK+2.D0*k3KK+k4KK)/6.D0
dzRK4k1=(k1k1+2.D0*k2k1+2.D0*k3k1+k4k1)/6.D0
dzRK4k2=(k1k2+2.D0*k2k2+2.D0*k3k2+k4k2)/6.D0
dzRK4Nh=(k1Nh+2.D0*k2Nh+2.D0*k3Nh+k4Nh)/6.D0

END SUBROUTINE dzRK4

END MODULE module222dzRK4
