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
USE module001FunctionsVarHldev
USE module002AnalDerivs
USE module002FunctionsEvAll
USE module002FunctionsEvAlldev
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
REAL(8),DIMENSION(Nx,Ny) :: H1dev,H1tdev,H1ttdev,H1txdev,H1tydev,H1tzdev,H1xdev,H1xxdev,H1xydev,H1xzdev
REAL(8),DIMENSION(Nx,Ny) :: H1ydev,H1yydev,H1yzdev,H1zdev,H1zzdev
REAL(8),DIMENSION(Nx,Ny) :: H2dev,H2tdev,H2ttdev,H2txdev,H2tydev,H2tzdev,H2xdev,H2xxdev,H2xydev,H2xzdev
REAL(8),DIMENSION(Nx,Ny) :: H2ydev,H2yydev,H2yzdev,H2zdev,H2zzdev
REAL(8),DIMENSION(Nx,Ny) :: H1H2dev
REAL(8),DIMENSION(Nx,Ny) :: l11dev,l11tdev,l11ttdev,l11txdev,l11tydev,l11tzdev,l11xdev,l11xxdev,l11xydev,l11xzdev
REAL(8),DIMENSION(Nx,Ny) :: l11ydev,l11yydev,l11yzdev,l11zdev,l11zzdev
REAL(8),DIMENSION(Nx,Ny) :: l12dev,l12tdev,l12ttdev,l12txdev,l12tydev,l12tzdev,l12xdev,l12xxdev,l12xydev,l12xzdev
REAL(8),DIMENSION(Nx,Ny) :: l12ydev,l12yydev,l12yzdev,l12zdev,l12zzdev
REAL(8),DIMENSION(Nx,Ny) :: l13dev,l13tdev,l13ttdev,l13txdev,l13tydev,l13tzdev,l13xdev,l13xxdev,l13xydev,l13xzdev
REAL(8),DIMENSION(Nx,Ny) :: l13ydev,l13yydev,l13yzdev,l13zdev,l13zzdev
REAL(8),DIMENSION(Nx,Ny) :: l21dev,l21tdev,l21ttdev,l21txdev,l21tydev,l21tzdev,l21xdev,l21xxdev,l21xydev,l21xzdev
REAL(8),DIMENSION(Nx,Ny) :: l21ydev,l21yydev,l21yzdev,l21zdev,l21zzdev
REAL(8),DIMENSION(Nx,Ny) :: l22dev,l22tdev,l22ttdev,l22txdev,l22tydev,l22tzdev,l22xdev,l22xxdev,l22xydev,l22xzdev
REAL(8),DIMENSION(Nx,Ny) :: l22ydev,l22yydev,l22yzdev,l22zdev,l22zzdev
REAL(8),DIMENSION(Nx,Ny) :: l23dev,l23tdev,l23ttdev,l23txdev,l23tydev,l23tzdev,l23xdev,l23xxdev,l23xydev,l23xzdev
REAL(8),DIMENSION(Nx,Ny) :: l23ydev,l23yydev,l23yzdev,l23zdev,l23zzdev
REAL(8),DIMENSION(Nx,Ny) :: l11n1dev,l21n2dev,l11n12dev,l21n22dev,l112dev,l122dev,l132dev,l212dev,l222dev,l232dev
REAL(8),DIMENSION(Nx,Ny) :: f1dev,f2dev,f3dev,f4dev,f5dev,f6dev,f7dev,f8dev,f9dev,f10dev
REAL(8),DIMENSION(Nx,Ny) :: f11dev,f12dev,f13dev,f14dev,f15dev,f16dev,f17dev,f18dev,f19dev,f20dev
REAL(8),DIMENSION(Nx,Ny) :: f21dev,f22dev,f23dev,f24dev,f25dev,f26dev,f27dev,f28dev,f29dev,f30dev
REAL(8),DIMENSION(Nx,Ny) :: f31dev,f32dev,f33dev,f34dev,f35dev,f36dev,f37dev,f38dev,f39dev,f40dev
REAL(8),DIMENSION(Nx,Ny) :: f41dev,f42dev,f43dev,f44dev,f45dev,f46dev,f47dev,f48dev,f49dev,f50dev
REAL(8),DIMENSION(Nx,Ny) :: f51dev,f52dev,f53dev,f54dev,f55dev,f56dev,f57dev,f58dev,f59dev,f60dev
REAL(8),DIMENSION(Nx,Ny) :: f61dev,f62dev,f63dev,f64dev,f65dev,f66dev,f67dev,f68dev,f69dev,f70dev
REAL(8),DIMENSION(Nx,Ny) :: f71dev,f72dev,f73dev,f74dev,f75dev,f76dev,f77dev,f78dev,f79dev,f80dev
REAL(8),DIMENSION(Nx,Ny) :: f81dev,f82dev,f83dev,f84dev,f85dev,f86dev,f87dev,f88dev,f89dev,f90dev
REAL(8),DIMENSION(Nx,Ny) :: f91dev,f92dev,f93dev,f94dev,f95dev,f96dev,f97dev,f98dev,f99dev,f100dev
REAL(8),DIMENSION(Nx,Ny) :: f101dev,f102dev,f103dev,f104dev,f105dev,f106dev,f107dev,f108dev,f109dev,f110dev
REAL(8),DIMENSION(Nx,Ny) :: f111dev,f112dev,f113dev,f114dev,f115dev,f116dev,f117dev,f118dev,f119dev
REAL(8),DIMENSION(Nx,Ny) :: ff1dev,ff2dev,ff3dev,ff4dev,ff5dev,ff6dev,ff7dev,ff8dev,ff9dev,ff10dev
REAL(8),DIMENSION(Nx,Ny) :: ff11dev,ff12dev,ff13dev,ff14dev,ff15dev,ff16dev,ff17dev,ff18dev,ff19dev,ff20dev
REAL(8),DIMENSION(Nx,Ny) :: ff21dev,ff22dev,ff23dev,ff24dev,ff25dev,ff26dev,ff27dev,ff28dev,ff29dev,ff30dev
REAL(8),DIMENSION(Nx,Ny) :: ff31dev,ff32dev,ff33dev,ff34dev,ff35dev
REAL(8),DIMENSION(Nx,Ny) :: G1_mod,G2_mod,G3_mod,G4_mod,G5_mod,G6_mod,G7_mod,G8_mod,dxG1_mod,dyG1_mod,dxG2_mod,dyG2_mod
REAL(8),DIMENSION(Nx,Ny) :: a_mod,d_mod,kappa_mod,Kstar_mod,dxkappa_mod,dykappa_mod
REAL(8),DIMENSION(Nx,Ny) :: k1mod,k2mod,Nhmod,KKmod
REAL(8),DIMENSION(Nx,Ny) :: k1dev,k2dev,Nhdev,KKdev
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

!******* fkcje a,d,kappa,Kstar,G1-8 oraz pochodne kappa,G1,G2 na siatce (x,y), z uwzglednieniem zmod *******
DO i=1,Nx
 DO j=1,Ny
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
   CALL VarFuncHldev(x(i),y(j),zmod,&
    !OUT:
    H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
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
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j))
   !
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
   k1mod(i,j)=k1dev(i,j)-Ak1_mod(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2_mod(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh_mod(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK_mod(i,j)
END DO
i=Nx
DO j=1,Ny
   CALL VarFuncHldev(x(i),y(j),zmod,&
    !OUT:
    H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
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
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j))
   !
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
   k1mod(i,j)=k1dev(i,j)-Ak1_mod(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2_mod(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh_mod(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK_mod(i,j)
END DO
!
j=1
DO i=2,Nx-1
   CALL VarFuncHldev(x(i),y(j),zmod,&
    !OUT:
    H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
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
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j))
   !
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
   k1mod(i,j)=k1dev(i,j)-Ak1_mod(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2_mod(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh_mod(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK_mod(i,j)
END DO
j=Ny
DO i=2,Nx-1
   CALL VarFuncHldev(x(i),y(j),zmod,&
    !OUT:
    H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
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
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j))
   !
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
   k1mod(i,j)=k1dev(i,j)-Ak1_mod(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2_mod(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh_mod(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK_mod(i,j)
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
!******* fkcje pomocnicze H i l dev na siatce (x,y), z uwzglednieniem zmod *******
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
   k1mod(i,j)=k1dev(i,j)-Ak1_mod(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2_mod(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh_mod(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK_mod(i,j)
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
   k1mod(i,j)=k1dev(i,j)-Ak1_mod(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2_mod(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh_mod(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK_mod(i,j)
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
   k1mod(i,j)=k1dev(i,j)-Ak1_mod(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2_mod(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh_mod(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK_mod(i,j)
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
   k1mod(i,j)=k1dev(i,j)-Ak1_mod(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2_mod(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh_mod(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK_mod(i,j)
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
   CALL VarFuncHldev(x(i),y(j),z,&
    !OUT:
    H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
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
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j))
   !
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
   k1mod(i,j)=k1dev(i,j)-Ak1(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK(i,j)
END DO
i=Nx
DO j=1,Ny
   CALL VarFuncHldev(x(i),y(j),z,&
    !OUT:
    H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
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
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j))
   !
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
   k1mod(i,j)=k1dev(i,j)-Ak1(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK(i,j)
END DO
!
j=1
DO i=2,Nx-1
   CALL VarFuncHldev(x(i),y(j),z,&
    !OUT:
    H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
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
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j))
   !
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
   k1mod(i,j)=k1dev(i,j)-Ak1(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK(i,j)
END DO
j=Ny
DO i=2,Nx-1
   CALL VarFuncHldev(x(i),y(j),z,&
    !OUT:
    H1dev(i,j),H1tdev(i,j),H1ttdev(i,j),H1txdev(i,j),H1tydev(i,j),H1tzdev(i,j),&
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
    ff31dev(i,j),ff32dev(i,j),ff33dev(i,j),ff34dev(i,j),ff35dev(i,j))
   !
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
   k1mod(i,j)=k1dev(i,j)-Ak1(i,j)
   k2mod(i,j)=k2dev(i,j)-Ak2(i,j)
   Nhmod(i,j)=Nhdev(i,j)-ANh(i,j)
   KKmod(i,j)=KKdev(i,j)-AKK(i,j)
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
