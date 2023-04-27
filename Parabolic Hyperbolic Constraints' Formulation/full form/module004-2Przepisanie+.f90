MODULE module004Przepisanie

CONTAINS
SUBROUTINE Przepisanie(H1,H1t,H1tt,H1tx,H1ty,H1tz,H1x,H1xx,H1xy,H1xz,H1y,H1yy,H1yz,H1z,H1zz,&
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
!
IMPLICIT NONE
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
REAL(8),INTENT(IN) :: z
REAL(8),DIMENSION(:,:),INTENT(OUT) :: H1pz,H1tpz,H1ttpz,H1txpz,H1typz,H1tzpz,H1xpz,H1xxpz,H1xypz,H1xzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: H1ypz,H1yypz,H1yzpz,H1zpz,H1zzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: H2pz,H2tpz,H2ttpz,H2txpz,H2typz,H2tzpz,H2xpz,H2xxpz,H2xypz,H2xzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: H2ypz,H2yypz,H2yzpz,H2zpz,H2zzpz,H1H2pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l11pz,l11tpz,l11ttpz,l11txpz,l11typz,l11tzpz,l11xpz,l11xxpz,l11xypz,l11xzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l11ypz,l11yypz,l11yzpz,l11zpz,l11zzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l12pz,l12tpz,l12ttpz,l12txpz,l12typz,l12tzpz,l12xpz,l12xxpz,l12xypz,l12xzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l12ypz,l12yypz,l12yzpz,l12zpz,l12zzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l13pz,l13tpz,l13ttpz,l13txpz,l13typz,l13tzpz,l13xpz,l13xxpz,l13xypz,l13xzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l13ypz,l13yypz,l13yzpz,l13zpz,l13zzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l21pz,l21tpz,l21ttpz,l21txpz,l21typz,l21tzpz,l21xpz,l21xxpz,l21xypz,l21xzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l21ypz,l21yypz,l21yzpz,l21zpz,l21zzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l22pz,l22tpz,l22ttpz,l22txpz,l22typz,l22tzpz,l22xpz,l22xxpz,l22xypz,l22xzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l22ypz,l22yypz,l22yzpz,l22zpz,l22zzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l23pz,l23tpz,l23ttpz,l23txpz,l23typz,l23tzpz,l23xpz,l23xxpz,l23xypz,l23xzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l23ypz,l23yypz,l23yzpz,l23zpz,l23zzpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: l11n1pz,l21n2pz,l11n12pz,l21n22pz,l112pz,l122pz,l132pz,l212pz,l222pz,l232pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: f1pz,f2pz,f3pz,f4pz,f5pz,f6pz,f7pz,f8pz,f9pz,f10pz,f11pz,f12pz,f13pz,f14pz,f15pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: f16pz,f17pz,f18pz,f19pz,f20pz,f21pz,f22pz,f23pz,f24pz,f25pz,f26pz,f27pz,f28pz,f29pz,f30pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: f31pz,f32pz,f33pz,f34pz,f35pz,f36pz,f37pz,f38pz,f39pz,f40pz,f41pz,f42pz,f43pz,f44pz,f45pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: f46pz,f47pz,f48pz,f49pz,f50pz,f51pz,f52pz,f53pz,f54pz,f55pz,f56pz,f57pz,f58pz,f59pz,f60pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: f61pz,f62pz,f63pz,f64pz,f65pz,f66pz,f67pz,f68pz,f69pz,f70pz,f71pz,f72pz,f73pz,f74pz,f75pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: f76pz,f77pz,f78pz,f79pz,f80pz,f81pz,f82pz,f83pz,f84pz,f85pz,f86pz,f87pz,f88pz,f89pz,f90pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: f91pz,f92pz,f93pz,f94pz,f95pz,f96pz,f97pz,f98pz,f99pz,f100pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: f101pz,f102pz,f103pz,f104pz,f105pz,f106pz,f107pz,f108pz,f109pz,f110pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: f111pz,f112pz,f113pz,f114pz,f115pz,f116pz,f117pz,f118pz,f119pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: ff1pz,ff2pz,ff3pz,ff4pz,ff5pz,ff6pz,ff7pz,ff8pz,ff9pz,ff10pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: ff11pz,ff12pz,ff13pz,ff14pz,ff15pz,ff16pz,ff17pz,ff18pz,ff19pz,ff20pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: ff21pz,ff22pz,ff23pz,ff24pz,ff25pz,ff26pz,ff27pz,ff28pz,ff29pz,ff30pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: ff31pz,ff32pz,ff33pz,ff34pz,ff35pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: G1pz,G2pz,G3pz,G4pz,G5pz,G6pz,G7pz,G8pz,dxG1pz,dyG1pz,dxG2pz,dyG2pz,apz,dpz,kappapz,Kstarpz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: dxkappapz,dykappapz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: Ak1pz,Ak2pz,ANhpz,AKKpz,AdxNhpz,AdyNhpz,AdxKKpz,AdyKKpz,Adxk1pz,Adyk1pz,Adxk2pz,Adyk2pz
REAL(8),DIMENSION(:,:),INTENT(OUT) :: AdxxNhpz,AdyyNhpz,AdxyNhpz
REAL(8),INTENT(OUT) :: zpz

H1pz=H1
H1tpz=H1t
H1ttpz=H1tt
H1txpz=H1tx
H1typz=H1ty
H1tzpz=H1tz
H1xpz=H1x
H1xxpz=H1xx
H1xypz=H1xy
H1xzpz=H1xz
H1ypz=H1y
H1yypz=H1yy
H1yzpz=H1yz
H1zpz=H1z
H1zzpz=H1zz

H2pz=H2
H2tpz=H2t
H2ttpz=H2tt
H2txpz=H2tx
H2typz=H2ty
H2tzpz=H2tz
H2xpz=H2x
H2xxpz=H2xx
H2xypz=H2xy
H2xzpz=H2xz
H2ypz=H2y
H2yypz=H2yy
H2yzpz=H2yz
H2zpz=H2z
H2zzpz=H2zz

H1H2pz=H1H2

l11pz=l11
l11tpz=l11t
l11ttpz=l11tt
l11txpz=l11tx
l11typz=l11ty
l11tzpz=l11tz
l11xpz=l11x
l11xxpz=l11xx
l11xypz=l11xy
l11xzpz=l11xz
l11ypz=l11y
l11yypz=l11yy
l11yzpz=l11yz
l11zpz=l11z
l11zzpz=l11zz

l12pz=l12
l12tpz=l12t
l12ttpz=l12tt
l12txpz=l12tx
l12typz=l12ty
l12tzpz=l12tz
l12xpz=l12x
l12xxpz=l12xx
l12xypz=l12xy
l12xzpz=l12xz
l12ypz=l12y
l12yypz=l12yy
l12yzpz=l12yz
l12zpz=l12z
l12zzpz=l12zz

l13pz=l13
l13tpz=l13t
l13ttpz=l13tt
l13txpz=l13tx
l13typz=l13ty
l13tzpz=l13tz
l13xpz=l13x
l13xxpz=l13xx
l13xypz=l13xy
l13xzpz=l13xz
l13ypz=l13y
l13yypz=l13yy
l13yzpz=l13yz
l13zpz=l13z
l13zzpz=l13zz

l21pz=l21
l21tpz=l21t
l21ttpz=l21tt
l21txpz=l21tx
l21typz=l21ty
l21tzpz=l21tz
l21xpz=l21x
l21xxpz=l21xx
l21xypz=l21xy
l21xzpz=l21xz
l21ypz=l21y
l21yypz=l21yy
l21yzpz=l21yz
l21zpz=l21z
l21zzpz=l21zz

l22pz=l22
l22tpz=l22t
l22ttpz=l22tt
l22txpz=l22tx
l22typz=l22ty
l22tzpz=l22tz
l22xpz=l22x
l22xxpz=l22xx
l22xypz=l22xy
l22xzpz=l22xz
l22ypz=l22y
l22yypz=l22yy
l22yzpz=l22yz
l22zpz=l22z
l22zzpz=l22zz

l23pz=l23
l23tpz=l23t
l23ttpz=l23tt
l23txpz=l23tx
l23typz=l23ty
l23tzpz=l23tz
l23xpz=l23x
l23xxpz=l23xx
l23xypz=l23xy
l23xzpz=l23xz
l23ypz=l23y
l23yypz=l23yy
l23yzpz=l23yz
l23zpz=l23z
l23zzpz=l23zz

l11n1pz=l11n1
l21n2pz=l21n2

l11n12pz=l11n12
l21n22pz=l21n22

l112pz=l112
l122pz=l122
l132pz=l132
l212pz=l212
l222pz=l222
l232pz=l232

!-----------------------------------------------------------------------------------------------------------------------------

f1pz=f1
f2pz=f2
f3pz=f3
f4pz=f4

f5pz=f5
f6pz=f6
f7pz=f7
f8pz=f8

f9pz=f9
f10pz=f10
f11pz=f11
f12pz=f12

f13pz=f13
f14pz=f14
f15pz=f15

f16pz=f16
f17pz=f17
f18pz=f18
f19pz=f19
f20pz=f20
f21pz=f21
f22pz=f22
f23pz=f23
f24pz=f24

f25pz=f25
f26pz=f26
f27pz=f27


f28pz=f28
f29pz=f29
f30pz=f30
f31pz=f31
f32pz=f32
f33pz=f33
f34pz=f34
f35pz=f35
f36pz=f36

f37pz=f37
f38pz=f38
f39pz=f39
f40pz=f40

f41pz=f41
f42pz=f42
f43pz=f43
f44pz=f44


f45pz=f45
f46pz=f46
f47pz=f47
f48pz=f48
f49pz=f49
f50pz=f50
f51pz=f51
f52pz=f52
f53pz=f53

f54pz=f54
f55pz=f55
f56pz=f56
f57pz=f57
f58pz=f58
f59pz=f59
f60pz=f60
f61pz=f61
f62pz=f62
f63pz=f63

f64pz=f64
f65pz=f65
f66pz=f66
f67pz=f67
f68pz=f68
f69pz=f69
f70pz=f70
f71pz=f71
f72pz=f72


f73pz=f73
f74pz=f74
f75pz=f75
f76pz=f76
f77pz=f77
f78pz=f78
f79pz=f79
f80pz=f80

f81pz=f81
f82pz=f82
f83pz=f83
f84pz=f84
f85pz=f85
f86pz=f86
f87pz=f87
f88pz=f88
f89pz=f89

f90pz=f90
f91pz=f91
f92pz=f92

f93pz=f93
f94pz=f94
f95pz=f95
f96pz=f96
f97pz=f97
f98pz=f98
f99pz=f99
f100pz=f100
f101pz=f101

f102pz=f102
f103pz=f103
f104pz=f104
f105pz=f105
f106pz=f106
f107pz=f107
f108pz=f108
f109pz=f109
f110pz=f110

f111pz=f111
f112pz=f112
f113pz=f113
f114pz=f114
f115pz=f115
f116pz=f116
f117pz=f117
f118pz=f118
f119pz=f119


ff1pz=ff1
ff2pz=ff2
ff3pz=ff3
ff4pz=ff4
ff5pz=ff5

ff6pz=ff6
ff7pz=ff7
ff8pz=ff8
ff9pz=ff9
ff10pz=ff10
ff11pz=ff11

ff12pz=ff12
ff13pz=ff13
ff14pz=ff14

ff15pz=ff15
ff16pz=ff16
ff17pz=ff17

ff18pz=ff18
ff19pz=ff19
ff20pz=ff20
ff21pz=ff21
ff22pz=ff22
ff23pz=ff23
ff24pz=ff24
ff25pz=ff25
ff26pz=ff26
ff27pz=ff27

ff28pz=ff28
ff29pz=ff29

ff30pz=ff30
ff31pz=ff31
ff32pz=ff32
ff33pz=ff33
ff34pz=ff34
ff35pz=ff35

!-----------------------------------------------------------------------------------------------------------------------------

G1pz=G1
G2pz=G2
G3pz=G3
G4pz=G4
G5pz=G5
G6pz=G6
G7pz=G7
G8pz=G8
dxG1pz=dxG1
dyG1pz=dyG1
dxG2pz=dxG2
dyG2pz=dyG2
apz=a
dpz=d
kappapz=kappa
Kstarpz=Kstar
dxkappapz=dxkappa
dykappapz=dykappa

Ak1pz=Ak1
Ak2pz=Ak2
ANhpz=ANh
AKKpz=AKK
AdxNhpz=AdxNh
AdyNhpz=AdyNh
AdxKKpz=AdxKK
AdyKKpz=AdyKK
Adxk1pz=Adxk1
Adyk1pz=Adyk1
Adxk2pz=Adxk2
Adyk2pz=Adyk2
AdxxNhpz=AdxxNh
AdyyNhpz=AdyyNh
AdxyNhpz=AdxyNh

zpz=z

END SUBROUTINE Przepisanie

END MODULE module004Przepisanie
