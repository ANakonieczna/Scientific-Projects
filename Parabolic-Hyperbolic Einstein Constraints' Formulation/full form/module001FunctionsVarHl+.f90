MODULE module001FunctionsVarHl

CONTAINS
SUBROUTINE VarFuncHl(x,y,z,&
 !OUT:
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
 ff21,ff22,ff23,ff24,ff25,ff26,ff27,ff28,ff29,ff30,ff31,ff32,ff33,ff34,ff35)
!
IMPLICIT NONE
!******* PARAMETRY *******
REAL(8) :: M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy
INTEGER :: Nx,Ny
REAL(8) :: p2
COMMON /PARAMS/ M1,a1,n1,d1,M2,a2,n2,d2,n12,n22,a12,a22,n1n2,mn1,mn2,sq1,sq2,sq12,r1,r2,U,zINI,dz,hx,hy,Nx,Ny,p2
!*************************
REAL(8),INTENT(IN) :: x,y,z
REAL(8),INTENT(OUT) :: H1,H1t,H1tt,H1tx,H1ty,H1tz,H1x,H1xx,H1xy,H1xz,H1y,H1yy,H1yz,H1z,H1zz
REAL(8),INTENT(OUT) :: H2,H2t,H2tt,H2tx,H2ty,H2tz,H2x,H2xx,H2xy,H2xz,H2y,H2yy,H2yz,H2z,H2zz,H1H2
REAL(8),INTENT(OUT) :: l11,l11t,l11tt,l11tx,l11ty,l11tz,l11x,l11xx,l11xy,l11xz,l11y,l11yy,l11yz,l11z,l11zz
REAL(8),INTENT(OUT) :: l12,l12t,l12tt,l12tx,l12ty,l12tz,l12x,l12xx,l12xy,l12xz,l12y,l12yy,l12yz,l12z,l12zz
REAL(8),INTENT(OUT) :: l13,l13t,l13tt,l13tx,l13ty,l13tz,l13x,l13xx,l13xy,l13xz,l13y,l13yy,l13yz,l13z,l13zz
REAL(8),INTENT(OUT) :: l21,l21t,l21tt,l21tx,l21ty,l21tz,l21x,l21xx,l21xy,l21xz,l21y,l21yy,l21yz,l21z,l21zz
REAL(8),INTENT(OUT) :: l22,l22t,l22tt,l22tx,l22ty,l22tz,l22x,l22xx,l22xy,l22xz,l22y,l22yy,l22yz,l22z,l22zz
REAL(8),INTENT(OUT) :: l23,l23t,l23tt,l23tx,l23ty,l23tz,l23x,l23xx,l23xy,l23xz,l23y,l23yy,l23yz,l23z,l23zz
REAL(8),INTENT(OUT) :: l11n1,l21n2,l11n12,l21n22,l112,l122,l132,l212,l222,l232
REAL(8),INTENT(OUT) :: f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20
REAL(8),INTENT(OUT) :: f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36,f37,f38,f39,f40
REAL(8),INTENT(OUT) :: f41,f42,f43,f44,f45,f46,f47,f48,f49,f50,f51,f52,f53,f54,f55,f56,f57,f58,f59,f60
REAL(8),INTENT(OUT) :: f61,f62,f63,f64,f65,f66,f67,f68,f69,f70,f71,f72,f73,f74,f75,f76,f77,f78,f79,f80
REAL(8),INTENT(OUT) :: f81,f82,f83,f84,f85,f86,f87,f88,f89,f90,f91,f92,f93,f94,f95,f96,f97,f98,f99,f100
REAL(8),INTENT(OUT) :: f101,f102,f103,f104,f105,f106,f107,f108,f109,f110,f111,f112,f113,f114,f115,f116,f117,f118,f119
REAL(8),INTENT(OUT) :: ff1,ff2,ff3,ff4,ff5,ff6,ff7,ff8,ff9,ff10,ff11,ff12,ff13,ff14,ff15,ff16,ff17,ff18,ff19,ff20
REAL(8),INTENT(OUT) :: ff21,ff22,ff23,ff24,ff25,ff26,ff27,ff28,ff29,ff30,ff31,ff32,ff33,ff34,ff35
REAL(8) :: x2,z2,d1y,d2y,d1y2,d2y2

x2=x*x
z2=z*z

d1y=d1-y
d2y=d2+y
d1y2=(d1-y)**2
d2y2=(d2+y)**2

ff30=4*a12*z2+(-a12+d1y2+x2/mn1+z2)**2
ff31=-a12+d1y2+x2/mn1+z2
ff32=4*a22*z2+(-a22+d2y2+x2/mn2+z2)**2
ff33=-a22+d2y2+x2/mn2+z2
ff34=DSQRT(ff30)
ff35=DSQRT(ff32)

!-----------------------------------------------------------------------------------------------------------------------------

H1=&
(DSQRT(ff31+ff34)*M1)/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))

H1t=&
(M1*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34)*((ff31+&
ff34)/2+(2*a12*z2)/(ff31+ff34)))-(DSQRT(ff31+ff34)*M1*(((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/2-&
(2*a12*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1tt=&
-(M1*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))**2)/(4*p2*(ff31+ff34)**1.5*((ff31+&
ff34)/2+(2*a12*z2)/(ff31+ff34)))+(M1*((2*n12)/mn1+(2*ff31*n12)/(DSQRT(ff30)*mn1)+&
(4*n12*x2)/(DSQRT(ff30)*mn1**2)-(4*ff31**2*n12*x2)/(ff30**1.5*mn1**2)))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))-(M1*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(((-&
2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+&
ff34)**2))/(p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)+(p2*DSQRT(ff31+&
ff34)*M1*(((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2)**2)/((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34))**3-(DSQRT(ff31+ff34)*M1*(((2*n12)/mn1+(2*ff31*n12)/(DSQRT(ff30)*mn1)+(4*n12*x2)/(DSQRT(ff30)*mn1**2)-&
(4*ff31**2*n12*x2)/(ff30**1.5*mn1**2))/2+(4*a12*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))**2*z2)/(ff31+&
ff34)**3-(2*a12*((2*n12)/mn1+(2*ff31*n12)/(DSQRT(ff30)*mn1)+(4*n12*x2)/(DSQRT(ff30)*mn1**2)-&
(4*ff31**2*n12*x2)/(ff30**1.5*mn1**2))*z2)/(ff31+ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1tx=&
-(M1*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(4*p2*(ff31+&
ff34)**1.5*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))+(M1*((-2*n1)/mn1-(2*ff31*n1)/(DSQRT(ff30)*mn1)-&
(4*n1*x2)/(DSQRT(ff30)*mn1**2)+(4*ff31**2*n1*x2)/(ff30**1.5*mn1**2)))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))-(M1*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+&
ff34)**2))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)-(M1*((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1))*(((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/2-&
(2*a12*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)+(p2*DSQRT(ff31+ff34)*M1*(((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+&
ff34)**2)*(((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2))/((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34))**3-(DSQRT(ff31+ff34)*M1*(((-2*n1)/mn1-(2*ff31*n1)/(DSQRT(ff30)*mn1)-(4*n1*x2)/(DSQRT(ff30)*mn1**2)+&
(4*ff31**2*n1*x2)/(ff30**1.5*mn1**2))/2+(4*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*((-&
2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**3-(2*a12*((-2*n1)/mn1-&
(2*ff31*n1)/(DSQRT(ff30)*mn1)-(4*n1*x2)/(DSQRT(ff30)*mn1**2)+(4*ff31**2*n1*x2)/(ff30**1.5*mn1**2))*z2)/(ff31+&
ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1ty=&
-(M1*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))/2-&
(2*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z2)/(ff31+ff34)**2))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*M1*((-&
2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(4*p2*(ff31+ff34)**1.5*((ff31+ff34)/2+&
(2*a12*z2)/(ff31+ff34)))+(M1*((4*d1y*n1*x)/(DSQRT(ff30)*mn1)-(4*d1y*ff31**2*n1*x)/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))+(p2*DSQRT(ff31+ff34)*M1*((-2*d1y-&
(2*d1y*ff31)/DSQRT(ff30))/2-(2*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z2)/(ff31+&
ff34)**2)*(((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2))/((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34))**3-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*M1*(((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/2-&
(2*a12*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)-(DSQRT(ff31+ff34)*M1*(((4*d1y*n1*x)/(DSQRT(ff30)*mn1)-&
(4*d1y*ff31**2*n1*x)/(ff30**1.5*mn1))/2+(4*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((-&
2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**3-(2*a12*((4*d1y*n1*x)/(DSQRT(ff30)*mn1)-&
(4*d1y*ff31**2*n1*x)/(ff30**1.5*mn1))*z2)/(ff31+ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1tz=&
-(M1*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(4*p2*(ff31+&
ff34)**1.5*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))-(M1*(((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/2-&
(2*a12*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2)*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34))**2)+(M1*((-4*n1*x*z)/(DSQRT(ff30)*mn1)+(ff31*n1*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))-(M1*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*((4*a12*z)/(ff31+&
ff34)+(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))/2-(2*a12*z2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+&
ff34)**2))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)+(p2*DSQRT(ff31+&
ff34)*M1*(((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2)*((4*a12*z)/(ff31+ff34)+(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))/2-(2*a12*z2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+&
ff34)**2))/((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**3-(DSQRT(ff31+ff34)*M1*((-4*a12*((-&
2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z)/(ff31+ff34)**2+(4*a12*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+&
ff34)**3+((-4*n1*x*z)/(DSQRT(ff30)*mn1)+(ff31*n1*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1))/2-&
(2*a12*z2*((-4*n1*x*z)/(DSQRT(ff30)*mn1)+(ff31*n1*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(ff31+&
ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1x=&
(M1*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+&
(2*a12*z2)/(ff31+ff34)))-(DSQRT(ff31+ff34)*M1*(((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))/2-&
(2*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1xx=&
-(M1*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))**2)/(4*p2*(ff31+ff34)**1.5*((ff31+&
ff34)/2+(2*a12*z2)/(ff31+ff34)))+(M1*(2/mn1+(2*ff31)/(DSQRT(ff30)*mn1)+(4*x2)/(DSQRT(ff30)*mn1**2)-&
(4*ff31**2*x2)/(ff30**1.5*mn1**2)))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34)))-(M1*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))/2-&
(2*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2))/(p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)+(p2*DSQRT(ff31+ff34)*M1*(((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+&
ff34)**2)**2)/((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**3-(DSQRT(ff31+ff34)*M1*((2/mn1+&
(2*ff31)/(DSQRT(ff30)*mn1)+(4*x2)/(DSQRT(ff30)*mn1**2)-(4*ff31**2*x2)/(ff30**1.5*mn1**2))/2+&
(4*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))**2*z2)/(ff31+ff34)**3-(2*a12*(2/mn1+&
(2*ff31)/(DSQRT(ff30)*mn1)+(4*x2)/(DSQRT(ff30)*mn1**2)-(4*ff31**2*x2)/(ff30**1.5*mn1**2))*z2)/(ff31+&
ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1xy=&
-(M1*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))/2-&
(2*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z2)/(ff31+ff34)**2))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*M1*((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1)))/(4*p2*(ff31+ff34)**1.5*((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34)))+(M1*((-4*d1y*x)/(DSQRT(ff30)*mn1)+(4*d1y*ff31**2*x)/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))+(p2*DSQRT(ff31+ff34)*M1*((-2*d1y-&
(2*d1y*ff31)/DSQRT(ff30))/2-(2*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z2)/(ff31+&
ff34)**2)*(((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+&
ff34)**2))/((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**3-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*M1*(((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+&
ff34)**2))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)-(DSQRT(ff31+&
ff34)*M1*(((-4*d1y*x)/(DSQRT(ff30)*mn1)+(4*d1y*ff31**2*x)/(ff30**1.5*mn1))/2+(4*a12*(-&
2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+&
ff34)**3-(2*a12*((-4*d1y*x)/(DSQRT(ff30)*mn1)+(4*d1y*ff31**2*x)/(ff30**1.5*mn1))*z2)/(ff31+&
ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1xz=&
-(M1*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(4*p2*(ff31+&
ff34)**1.5*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))-(M1*(((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))/2-&
(2*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+ff34)**2)*(2*z+(8*a12*z+&
4*ff31*z)/(2*DSQRT(ff30))))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34))**2)+(M1*((4*x*z)/(DSQRT(ff30)*mn1)-(ff31*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))-(M1*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*((4*a12*z)/(ff31+&
ff34)+(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))/2-(2*a12*z2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+&
ff34)**2))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)+(p2*DSQRT(ff31+&
ff34)*M1*(((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))/2-(2*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2)/(ff31+&
ff34)**2)*((4*a12*z)/(ff31+ff34)+(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))/2-(2*a12*z2*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+ff34)**2))/((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34))**3-(DSQRT(ff31+ff34)*M1*((-4*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z)/(ff31+&
ff34)**2+(4*a12*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+&
ff34)**3+((4*x*z)/(DSQRT(ff30)*mn1)-(ff31*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1))/2-&
(2*a12*z2*((4*x*z)/(DSQRT(ff30)*mn1)-(ff31*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(ff31+&
ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1y=&
-((DSQRT(ff31+ff34)*M1*((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))/2-(2*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z2)/(ff31+&
ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2))+((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*M1)/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))

H1yy=&
(p2*DSQRT(ff31+ff34)*M1*((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))/2-(2*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z2)/(ff31+&
ff34)**2)**2)/((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**3-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*M1*((-&
2*d1y-(2*d1y*ff31)/DSQRT(ff30))/2-(2*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z2)/(ff31+&
ff34)**2))/(p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)-(DSQRT(ff31+&
ff34)*M1*((2+(4*d1y2)/DSQRT(ff30)+(2*ff31)/DSQRT(ff30)-(4*d1y2*ff31**2)/ff30**1.5)/2+&
(4*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))**2*z2)/(ff31+ff34)**3-(2*a12*(2+(4*d1y2)/DSQRT(ff30)+&
(2*ff31)/DSQRT(ff30)-(4*d1y2*ff31**2)/ff30**1.5)*z2)/(ff31+ff34)**2))/(p2*((ff31+&
ff34)/2+(2*a12*z2)/(ff31+ff34))**2)-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))**2*M1)/(4*p2*(ff31+&
ff34)**1.5*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))+((2+(4*d1y2)/DSQRT(ff30)+(2*ff31)/DSQRT(ff30)-&
(4*d1y2*ff31**2)/ff30**1.5)*M1)/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))

H1yz=&
-(M1*((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))/2-(2*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z2)/(ff31+&
ff34)**2)*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(2*p2*DSQRT(ff31+ff34)*((ff31+&
ff34)/2+(2*a12*z2)/(ff31+ff34))**2)-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*M1*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(4*p2*(ff31+ff34)**1.5*((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34)))+(M1*((-4*d1y*z)/DSQRT(ff30)+(d1y*ff31*(8*a12*z+4*ff31*z))/ff30**1.5))/(2*p2*DSQRT(ff31+&
ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34)))+(p2*DSQRT(ff31+ff34)*M1*((-2*d1y-&
(2*d1y*ff31)/DSQRT(ff30))/2-(2*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z2)/(ff31+&
ff34)**2)*((4*a12*z)/(ff31+ff34)+(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))/2-(2*a12*z2*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+ff34)**2))/((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34))**3-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*M1*((4*a12*z)/(ff31+ff34)+(2*z+(8*a12*z+&
4*ff31*z)/(2*DSQRT(ff30)))/2-(2*a12*z2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+&
ff34)**2))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)-(DSQRT(ff31+&
ff34)*M1*((-4*a12*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z)/(ff31+ff34)**2+(4*a12*(-&
2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+&
ff34)**3+((-4*d1y*z)/DSQRT(ff30)+(d1y*ff31*(8*a12*z+4*ff31*z))/ff30**1.5)/2-(2*a12*z2*((-&
4*d1y*z)/DSQRT(ff30)+(d1y*ff31*(8*a12*z+4*ff31*z))/ff30**1.5))/(ff31+ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1z=&
(M1*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+&
(2*a12*z2)/(ff31+ff34)))-(DSQRT(ff31+ff34)*M1*((4*a12*z)/(ff31+ff34)+(2*z+(8*a12*z+&
4*ff31*z)/(2*DSQRT(ff30)))/2-(2*a12*z2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+&
ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)

H1zz=&
-(M1*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))**2)/(4*p2*(ff31+ff34)**1.5*((ff31+&
ff34)/2+(2*a12*z2)/(ff31+ff34)))+(M1*(2-(8*a12*z+4*ff31*z)**2/(4*ff30**1.5)+(8*a12+&
4*ff31+8*z2)/(2*DSQRT(ff30))))/(2*p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34)))-(M1*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))*((4*a12*z)/(ff31+ff34)+(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))/2-(2*a12*z2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+&
ff34)**2))/(p2*DSQRT(ff31+ff34)*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)+(p2*DSQRT(ff31+&
ff34)*M1*((4*a12*z)/(ff31+ff34)+(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))/2-(2*a12*z2*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+ff34)**2)**2)/((ff31+ff34)/2+(2*a12*z2)/(ff31+&
ff34))**3-(DSQRT(ff31+ff34)*M1*((4*a12)/(ff31+ff34)-(8*a12*z*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+&
ff34)**2+(4*a12*z2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))**2)/(ff31+ff34)**3+&
(2-(8*a12*z+4*ff31*z)**2/(4*ff30**1.5)+(8*a12+4*ff31+8*z2)/(2*DSQRT(ff30)))/2-(2*a12*z2*(2-&
(8*a12*z+4*ff31*z)**2/(4*ff30**1.5)+(8*a12+4*ff31+8*z2)/(2*DSQRT(ff30))))/(ff31+&
ff34)**2))/(p2*((ff31+ff34)/2+(2*a12*z2)/(ff31+ff34))**2)


H2=&
(DSQRT(ff33+ff35)*M2)/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))

H2t=&
(M2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+ff35)*((ff33+&
ff35)/2+(2*a22*z2)/(ff33+ff35)))-(DSQRT(ff33+ff35)*M2*(((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/2-&
(2*a22*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2tt=&
-(M2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))**2)/(4*p2*(ff33+ff35)**1.5*((ff33+&
ff35)/2+(2*a22*z2)/(ff33+ff35)))+(M2*((2*n22)/mn2+(2*ff33*n22)/(DSQRT(ff32)*mn2)+&
(4*n22*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*n22*x2)/(ff32**1.5*mn2**2)))/(2*p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))-(M2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/2-(2*a22*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+&
ff35)**2))/(p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)+(p2*DSQRT(ff33+&
ff35)*M2*(((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/2-(2*a22*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2)**2)/((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35))**3-(DSQRT(ff33+ff35)*M2*(((2*n22)/mn2+(2*ff33*n22)/(DSQRT(ff32)*mn2)+(4*n22*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*n22*x2)/(ff32**1.5*mn2**2))/2+(4*a22*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))**2*z2)/(ff33+&
ff35)**3-(2*a22*((2*n22)/mn2+(2*ff33*n22)/(DSQRT(ff32)*mn2)+(4*n22*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*n22*x2)/(ff32**1.5*mn2**2))*z2)/(ff33+ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2tx=&
-(M2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(4*p2*(ff33+&
ff35)**1.5*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))+(M2*((2*n2)/mn2+(2*ff33*n2)/(DSQRT(ff32)*mn2)+&
(4*n2*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*n2*x2)/(ff32**1.5*mn2**2)))/(2*p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))-(M2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2))/2-(2*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+&
ff35)**2))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)-(M2*((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2))*(((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/2-&
(2*a22*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2))/(2*p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)+(p2*DSQRT(ff33+ff35)*M2*(((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2))/2-(2*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+&
ff35)**2)*(((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/2-(2*a22*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2))/((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35))**3-(DSQRT(ff33+ff35)*M2*(((2*n2)/mn2+(2*ff33*n2)/(DSQRT(ff32)*mn2)+(4*n2*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*n2*x2)/(ff32**1.5*mn2**2))/2+(4*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**3-(2*a22*((2*n2)/mn2+(2*ff33*n2)/(DSQRT(ff32)*mn2)+&
(4*n2*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*n2*x2)/(ff32**1.5*mn2**2))*z2)/(ff33+&
ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2ty=&
-(M2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*((2*d2y+(2*d2y*ff33)/DSQRT(ff32))/2-&
(2*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z2)/(ff33+ff35)**2))/(2*p2*DSQRT(ff33+ff35)*((ff33+&
ff35)/2+(2*a22*z2)/(ff33+ff35))**2)-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*M2*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(4*p2*(ff33+ff35)**1.5*((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35)))+(M2*((4*d2y*n2*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*n2*x)/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))+(p2*DSQRT(ff33+ff35)*M2*((2*d2y+(2*d2y*ff33)/DSQRT(ff32))/2-&
(2*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z2)/(ff33+ff35)**2)*(((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/2-&
(2*a22*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2))/((ff33+&
ff35)/2+(2*a22*z2)/(ff33+ff35))**3-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*M2*(((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/2-(2*a22*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+&
ff35)**2))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)-(DSQRT(ff33+&
ff35)*M2*(((4*d2y*n2*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*n2*x)/(ff32**1.5*mn2))/2+&
(4*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+&
ff35)**3-(2*a22*((4*d2y*n2*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*n2*x)/(ff32**1.5*mn2))*z2)/(ff33+&
ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2tz=&
-(M2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(4*p2*(ff33+&
ff35)**1.5*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))-(M2*(((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/2-&
(2*a22*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2)*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35))**2)+(M2*((4*n2*x*z)/(DSQRT(ff32)*mn2)-(ff33*n2*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))-(M2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*((4*a22*z)/(ff33+&
ff35)+(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))/2-(2*a22*z2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+&
ff35)**2))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)+(p2*DSQRT(ff33+&
ff35)*M2*(((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/2-(2*a22*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2)*((4*a22*z)/(ff33+ff35)+(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))/2-(2*a22*z2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+&
ff35)**2))/((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**3-(DSQRT(ff33+ff35)*M2*((-4*a22*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z)/(ff33+ff35)**2+(4*a22*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z2*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+ff35)**3+((4*n2*x*z)/(DSQRT(ff32)*mn2)-&
(ff33*n2*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2))/2-(2*a22*z2*((4*n2*x*z)/(DSQRT(ff32)*mn2)-&
(ff33*n2*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(ff33+ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2x=&
(M2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+&
(2*a22*z2)/(ff33+ff35)))-(DSQRT(ff33+ff35)*M2*(((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))/2-&
(2*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2xx=&
-(M2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))**2)/(4*p2*(ff33+ff35)**1.5*((ff33+&
ff35)/2+(2*a22*z2)/(ff33+ff35)))+(M2*(2/mn2+(2*ff33)/(DSQRT(ff32)*mn2)+(4*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*x2)/(ff32**1.5*mn2**2)))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35)))-(M2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*(((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))/2-&
(2*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2))/(p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)+(p2*DSQRT(ff33+ff35)*M2*(((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2))/2-(2*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+&
ff35)**2)**2)/((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**3-(DSQRT(ff33+ff35)*M2*((2/mn2+&
(2*ff33)/(DSQRT(ff32)*mn2)+(4*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*x2)/(ff32**1.5*mn2**2))/2+&
(4*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))**2*z2)/(ff33+ff35)**3-(2*a22*(2/mn2+&
(2*ff33)/(DSQRT(ff32)*mn2)+(4*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*x2)/(ff32**1.5*mn2**2))*z2)/(ff33+&
ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2xy=&
-(M2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((2*d2y+(2*d2y*ff33)/DSQRT(ff32))/2-&
(2*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z2)/(ff33+ff35)**2))/(2*p2*DSQRT(ff33+ff35)*((ff33+&
ff35)/2+(2*a22*z2)/(ff33+ff35))**2)-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*M2*((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2)))/(4*p2*(ff33+ff35)**1.5*((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35)))+(M2*((4*d2y*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*x)/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))+(p2*DSQRT(ff33+ff35)*M2*((2*d2y+(2*d2y*ff33)/DSQRT(ff32))/2-&
(2*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z2)/(ff33+ff35)**2)*(((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))/2-&
(2*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2))/((ff33+ff35)/2+&
(2*a22*z2)/(ff33+ff35))**3-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*M2*(((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))/2-&
(2*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2))/(2*p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)-(DSQRT(ff33+ff35)*M2*(((4*d2y*x)/(DSQRT(ff32)*mn2)-&
(4*d2y*ff33**2*x)/(ff32**1.5*mn2))/2+(4*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**3-(2*a22*((4*d2y*x)/(DSQRT(ff32)*mn2)-&
(4*d2y*ff33**2*x)/(ff32**1.5*mn2))*z2)/(ff33+ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2xz=&
-(M2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(4*p2*(ff33+&
ff35)**1.5*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))-(M2*(((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))/2-&
(2*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+ff35)**2)*(2*z+(8*a22*z+&
4*ff33*z)/(2*DSQRT(ff32))))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35))**2)+(M2*((4*x*z)/(DSQRT(ff32)*mn2)-(ff33*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))-(M2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((4*a22*z)/(ff33+&
ff35)+(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))/2-(2*a22*z2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+&
ff35)**2))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)+(p2*DSQRT(ff33+&
ff35)*M2*(((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))/2-(2*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z2)/(ff33+&
ff35)**2)*((4*a22*z)/(ff33+ff35)+(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))/2-(2*a22*z2*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+ff35)**2))/((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35))**3-(DSQRT(ff33+ff35)*M2*((-4*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z)/(ff33+&
ff35)**2+(4*a22*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+&
ff35)**3+((4*x*z)/(DSQRT(ff32)*mn2)-(ff33*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2))/2-&
(2*a22*z2*((4*x*z)/(DSQRT(ff32)*mn2)-(ff33*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(ff33+&
ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2y=&
-((DSQRT(ff33+ff35)*M2*((2*d2y+(2*d2y*ff33)/DSQRT(ff32))/2-(2*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z2)/(ff33+&
ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2))+((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*M2)/(2*p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))

H2yy=&
(p2*DSQRT(ff33+ff35)*M2*((2*d2y+(2*d2y*ff33)/DSQRT(ff32))/2-(2*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z2)/(ff33+&
ff35)**2)**2)/((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**3-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*M2*((2*d2y+&
(2*d2y*ff33)/DSQRT(ff32))/2-(2*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z2)/(ff33+ff35)**2))/(p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)-(DSQRT(ff33+ff35)*M2*((2+(4*d2y2)/DSQRT(ff32)+&
(2*ff33)/DSQRT(ff32)-(4*d2y2*ff33**2)/ff32**1.5)/2+(4*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))**2*z2)/(ff33+&
ff35)**3-(2*a22*(2+(4*d2y2)/DSQRT(ff32)+(2*ff33)/DSQRT(ff32)-(4*d2y2*ff33**2)/ff32**1.5)*z2)/(ff33+&
ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))**2*M2)/(4*p2*(ff33+&
ff35)**1.5*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))+((2+(4*d2y2)/DSQRT(ff32)+(2*ff33)/DSQRT(ff32)-&
(4*d2y2*ff33**2)/ff32**1.5)*M2)/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))

H2yz=&
-(M2*((2*d2y+(2*d2y*ff33)/DSQRT(ff32))/2-(2*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z2)/(ff33+&
ff35)**2)*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(2*p2*DSQRT(ff33+ff35)*((ff33+&
ff35)/2+(2*a22*z2)/(ff33+ff35))**2)-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*M2*(2*z+(8*a22*z+&
4*ff33*z)/(2*DSQRT(ff32))))/(4*p2*(ff33+ff35)**1.5*((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35)))+(M2*((4*d2y*z)/DSQRT(ff32)-(d2y*ff33*(8*a22*z+4*ff33*z))/ff32**1.5))/(2*p2*DSQRT(ff33+&
ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35)))+(p2*DSQRT(ff33+ff35)*M2*((2*d2y+(2*d2y*ff33)/DSQRT(ff32))/2-&
(2*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z2)/(ff33+ff35)**2)*((4*a22*z)/(ff33+ff35)+&
(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))/2-(2*a22*z2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+&
ff35)**2))/((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**3-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*M2*((4*a22*z)/(ff33+&
ff35)+(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))/2-(2*a22*z2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+&
ff35)**2))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)-(DSQRT(ff33+&
ff35)*M2*((-4*a22*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z)/(ff33+ff35)**2+(4*a22*(2*d2y+&
(2*d2y*ff33)/DSQRT(ff32))*z2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+ff35)**3+&
((4*d2y*z)/DSQRT(ff32)-(d2y*ff33*(8*a22*z+4*ff33*z))/ff32**1.5)/2-(2*a22*z2*((4*d2y*z)/DSQRT(ff32)-&
(d2y*ff33*(8*a22*z+4*ff33*z))/ff32**1.5))/(ff33+ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2z=&
(M2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+&
(2*a22*z2)/(ff33+ff35)))-(DSQRT(ff33+ff35)*M2*((4*a22*z)/(ff33+ff35)+(2*z+(8*a22*z+&
4*ff33*z)/(2*DSQRT(ff32)))/2-(2*a22*z2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+&
ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)

H2zz=&
-(M2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))**2)/(4*p2*(ff33+ff35)**1.5*((ff33+&
ff35)/2+(2*a22*z2)/(ff33+ff35)))+(M2*(2-(8*a22*z+4*ff33*z)**2/(4*ff32**1.5)+(8*a22+&
4*ff33+8*z2)/(2*DSQRT(ff32))))/(2*p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35)))-(M2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))*((4*a22*z)/(ff33+ff35)+(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))/2-(2*a22*z2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+&
ff35)**2))/(p2*DSQRT(ff33+ff35)*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)+(p2*DSQRT(ff33+&
ff35)*M2*((4*a22*z)/(ff33+ff35)+(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))/2-(2*a22*z2*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+ff35)**2)**2)/((ff33+ff35)/2+(2*a22*z2)/(ff33+&
ff35))**3-(DSQRT(ff33+ff35)*M2*((4*a22)/(ff33+ff35)-(8*a22*z*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+&
ff35)**2+(4*a22*z2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))**2)/(ff33+ff35)**3+&
(2-(8*a22*z+4*ff33*z)**2/(4*ff32**1.5)+(8*a22+4*ff33+8*z2)/(2*DSQRT(ff32)))/2-(2*a22*z2*(2-&
(8*a22*z+4*ff33*z)**2/(4*ff32**1.5)+(8*a22+4*ff33+8*z2)/(2*DSQRT(ff32))))/(ff33+&
ff35)**2))/(p2*((ff33+ff35)/2+(2*a22*z2)/(ff33+ff35))**2)


H1H2=H1*H2

!-----------------------------------------------------------------------------------------------------------------------------

l11=&
(2*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11t=&
(-2*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(2*(-((DSQRT(ff31+ff34)*n1)/(p2*sq1))+(x*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11tt=&
(4*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))**2*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+&
d1y2+ff34+x2/mn1+z2)**3-(2*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1))*((2*n12)/mn1+&
(2*ff31*n12)/(DSQRT(ff30)*mn1)+(4*n12*x2)/(DSQRT(ff30)*mn1**2)-(4*ff31**2*n12*x2)/(ff30**1.5*mn1**2)))/(a12+&
d1y2+ff34+x2/mn1+z2)**2-(4*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(-((DSQRT(ff31+&
ff34)*n1)/(p2*sq1))+(x*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**2+(2*(-((n1*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(p2*DSQRT(ff31+&
ff34)*sq1))-(x*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))**2)/(4*p2*(ff31+&
ff34)**1.5*sq1)+(x*((2*n12)/mn1+(2*ff31*n12)/(DSQRT(ff30)*mn1)+(4*n12*x2)/(DSQRT(ff30)*mn1**2)-&
(4*ff31**2*n12*x2)/(ff30**1.5*mn1**2)))/(2*p2*DSQRT(ff31+ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11tx=&
(4*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(-&
(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**3-(2*(-(a1*d1y)+&
(DSQRT(ff31+ff34)*x)/(p2*sq1))*((-2*n1)/mn1-(2*ff31*n1)/(DSQRT(ff30)*mn1)-(4*n1*x2)/(DSQRT(ff30)*mn1**2)+&
(4*ff31**2*n1*x2)/(ff30**1.5*mn1**2)))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(DSQRT(ff31+ff34)/(p2*sq1)+(x*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(-&
((DSQRT(ff31+ff34)*n1)/(p2*sq1))+(x*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**2+(2*(-(n1*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*sq1)+((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/(2*p2*DSQRT(ff31+ff34)*sq1)-&
(x*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(4*p2*(ff31+&
ff34)**1.5*sq1)+(x*((-2*n1)/mn1-(2*ff31*n1)/(DSQRT(ff30)*mn1)-(4*n1*x2)/(DSQRT(ff30)*mn1**2)+&
(4*ff31**2*n1*x2)/(ff30**1.5*mn1**2)))/(2*p2*DSQRT(ff31+ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11ty=&
(4*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(-&
(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**3-(2*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(a1+((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*x)/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*((4*d1y*n1*x)/(DSQRT(ff30)*mn1)-(4*d1y*ff31**2*n1*x)/(ff30**1.5*mn1))*(-&
(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*(-2*d1y-&
(2*d1y*ff31)/DSQRT(ff30))*(-((DSQRT(ff31+ff34)*n1)/(p2*sq1))+(x*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34)*sq1)))/(a12+d1y2+ff34+&
x2/mn1+z2)**2+(2*(-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*n1)/(2*p2*DSQRT(ff31+ff34)*sq1)-&
((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*x*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(4*p2*(ff31+&
ff34)**1.5*sq1)+(x*((4*d1y*n1*x)/(DSQRT(ff30)*mn1)-(4*d1y*ff31**2*n1*x)/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11tz=&
(4*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1))*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+ff34+x2/mn1+z2)**3-(x*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(p2*DSQRT(ff31+&
ff34)*sq1*(a12+d1y2+ff34+x2/mn1+z2)**2)-(2*(-((DSQRT(ff31+ff34)*n1)/(p2*sq1))+(x*((-&
2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34)*sq1))*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*(-(a1*d1y)+&
(DSQRT(ff31+ff34)*x)/(p2*sq1))*((-4*n1*x*z)/(DSQRT(ff30)*mn1)+(ff31*n1*x*(8*a12*z+&
4*ff31*z))/(ff30**1.5*mn1)))/(a12+d1y2+ff34+x2/mn1+z2)**2+(2*(-(n1*(2*z+(8*a12*z+&
4*ff31*z)/(2*DSQRT(ff30))))/(2*p2*DSQRT(ff31+ff34)*sq1)-(x*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(4*p2*(ff31+ff34)**1.5*sq1)+(x*((-4*n1*x*z)/(DSQRT(ff30)*mn1)+&
(ff31*n1*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11x=&
(-2*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(2*(DSQRT(ff31+ff34)/(p2*sq1)+(x*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11xx=&
(4*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))**2*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+&
d1y2+ff34+x2/mn1+z2)**3-(2*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1))*(2/mn1+(2*ff31)/(DSQRT(ff30)*mn1)+&
(4*x2)/(DSQRT(ff30)*mn1**2)-(4*ff31**2*x2)/(ff30**1.5*mn1**2)))/(a12+d1y2+ff34+&
x2/mn1+z2)**2-(4*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(DSQRT(ff31+ff34)/(p2*sq1)+&
(x*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34)*sq1)))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(2*(((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))/(p2*DSQRT(ff31+&
ff34)*sq1)-(x*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))**2)/(4*p2*(ff31+ff34)**1.5*sq1)+&
(x*(2/mn1+(2*ff31)/(DSQRT(ff30)*mn1)+(4*x2)/(DSQRT(ff30)*mn1**2)-(4*ff31**2*x2)/(ff30**1.5*mn1**2)))/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11xy=&
(4*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(-&
(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**3-(2*((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1))*(a1+((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*x)/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*((-4*d1y*x)/(DSQRT(ff30)*mn1)+(4*d1y*ff31**2*x)/(ff30**1.5*mn1))*(-&
(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*(-2*d1y-&
(2*d1y*ff31)/DSQRT(ff30))*(DSQRT(ff31+ff34)/(p2*sq1)+(x*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**2+(2*((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))/(2*p2*DSQRT(ff31+&
ff34)*sq1)-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*x*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(4*p2*(ff31+&
ff34)**1.5*sq1)+(x*((-4*d1y*x)/(DSQRT(ff30)*mn1)+(4*d1y*ff31**2*x)/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11xz=&
(4*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1))*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+ff34+x2/mn1+z2)**3-(x*((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(p2*DSQRT(ff31+&
ff34)*sq1*(a12+d1y2+ff34+x2/mn1+z2)**2)-(2*(DSQRT(ff31+ff34)/(p2*sq1)+(x*((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34)*sq1))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+&
d1y2+ff34+x2/mn1+z2)**2-(2*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1))*((4*x*z)/(DSQRT(ff30)*mn1)-&
(ff31*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(a12+d1y2+ff34+x2/mn1+z2)**2+(2*((2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))/(2*p2*DSQRT(ff31+ff34)*sq1)-(x*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(4*p2*(ff31+ff34)**1.5*sq1)+(x*((4*x*z)/(DSQRT(ff30)*mn1)-&
(ff31*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11y=&
(-2*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(2*(a1+((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*x)/(2*p2*DSQRT(ff31+ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11yy=&
(4*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))**2*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+&
d1y2+ff34+x2/mn1+z2)**3-(4*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*(a1+((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*x)/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*(2+(4*d1y2)/DSQRT(ff30)+(2*ff31)/DSQRT(ff30)-&
(4*d1y2*ff31**2)/ff30**1.5)*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1)))/(a12+d1y2+&
ff34+x2/mn1+z2)**2+(2*(-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))**2*x)/(4*p2*(ff31+ff34)**1.5*sq1)+&
((2+(4*d1y2)/DSQRT(ff30)+(2*ff31)/DSQRT(ff30)-(4*d1y2*ff31**2)/ff30**1.5)*x)/(2*p2*DSQRT(ff31+&
ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11yz=&
(4*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1))*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+ff34+x2/mn1+z2)**3-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*x*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(p2*DSQRT(ff31+ff34)*sq1*(a12+d1y2+ff34+x2/mn1+&
z2)**2)-(2*(a1+((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*x)/(2*p2*DSQRT(ff31+ff34)*sq1))*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*(-(a1*d1y)+&
(DSQRT(ff31+ff34)*x)/(p2*sq1))*((-4*d1y*z)/DSQRT(ff30)+(d1y*ff31*(8*a12*z+4*ff31*z))/ff30**1.5))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(2*(-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*x*(2*z+(8*a12*z+&
4*ff31*z)/(2*DSQRT(ff30))))/(4*p2*(ff31+ff34)**1.5*sq1)+(x*((-4*d1y*z)/DSQRT(ff30)+&
(d1y*ff31*(8*a12*z+4*ff31*z))/ff30**1.5))/(2*p2*DSQRT(ff31+ff34)*sq1)))/(a12+d1y2+ff34+x2/mn1+z2)

l11z=&
(-2*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(x*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(p2*DSQRT(ff31+ff34)*sq1*(a12+d1y2+ff34+x2/mn1+z2))

l11zz=&
(4*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))**2)/(a12+&
d1y2+ff34+x2/mn1+z2)**3-(p2*x*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))**2)/(DSQRT(ff31+&
ff34)*sq1*(a12+d1y2+ff34+x2/mn1+z2)**2)-(x*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))**2)/(2*p2*(ff31+&
ff34)**1.5*sq1*(a12+d1y2+ff34+x2/mn1+z2))-(2*(-(a1*d1y)+(DSQRT(ff31+ff34)*x)/(p2*sq1))*(2-&
(8*a12*z+4*ff31*z)**2/(4*ff30**1.5)+(8*a12+4*ff31+8*z2)/(2*DSQRT(ff30))))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(x*(2-(8*a12*z+4*ff31*z)**2/(4*ff30**1.5)+(8*a12+4*ff31+&
8*z2)/(2*DSQRT(ff30))))/(p2*DSQRT(ff31+ff34)*sq1*(a12+d1y2+ff34+x2/mn1+z2))


l12=&
(2*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1))/(a12+d1y2+ff34+x2/mn1+z2)

l12t=&
(-2*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(-((d1y*DSQRT(ff31+ff34))/p2)-&
(a1*x)/sq1))/(a12+d1y2+ff34+x2/mn1+z2)**2+(2*((a1*n1)/sq1-(d1y*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12tt=&
(4*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))**2*(-((d1y*DSQRT(ff31+ff34))/p2)-&
(a1*x)/sq1))/(a12+d1y2+ff34+x2/mn1+z2)**3-(2*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1)*((2*n12)/mn1+&
(2*ff31*n12)/(DSQRT(ff30)*mn1)+(4*n12*x2)/(DSQRT(ff30)*mn1**2)-(4*ff31**2*n12*x2)/(ff30**1.5*mn1**2)))/(a12+&
d1y2+ff34+x2/mn1+z2)**2-(4*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*((a1*n1)/sq1-&
(d1y*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34))))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(2*((d1y*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))**2)/(4*p2*(ff31+&
ff34)**1.5)-(d1y*((2*n12)/mn1+(2*ff31*n12)/(DSQRT(ff30)*mn1)+(4*n12*x2)/(DSQRT(ff30)*mn1**2)-&
(4*ff31**2*n12*x2)/(ff30**1.5*mn1**2)))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12tx=&
(4*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(-&
((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1))/(a12+d1y2+ff34+x2/mn1+z2)**3-(2*(-((d1y*DSQRT(ff31+&
ff34))/p2)-(a1*x)/sq1)*((-2*n1)/mn1-(2*ff31*n1)/(DSQRT(ff30)*mn1)-(4*n1*x2)/(DSQRT(ff30)*mn1**2)+&
(4*ff31**2*n1*x2)/(ff30**1.5*mn1**2)))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(-(a1/sq1)-(d1y*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+&
ff34))))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*((a1*n1)/sq1-&
(d1y*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34))))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(2*((d1y*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(4*p2*(ff31+ff34)**1.5)-(d1y*((-2*n1)/mn1-(2*ff31*n1)/(DSQRT(ff30)*mn1)-&
(4*n1*x2)/(DSQRT(ff30)*mn1**2)+(4*ff31**2*n1*x2)/(ff30**1.5*mn1**2)))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12ty=&
(4*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(-&
((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1))/(a12+d1y2+ff34+x2/mn1+z2)**3-(2*(-(d1y*(-&
2*d1y-(2*d1y*ff31)/DSQRT(ff30)))/(2*p2*DSQRT(ff31+ff34))+DSQRT(ff31+ff34)/p2)*((-&
2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*((4*d1y*n1*x)/(DSQRT(ff30)*mn1)-&
(4*d1y*ff31**2*n1*x)/(ff30**1.5*mn1))*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1))/(a12+&
d1y2+ff34+x2/mn1+z2)**2-(2*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((a1*n1)/sq1-(d1y*((-&
2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+&
ff34+x2/mn1+z2)**2+(2*((d1y*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(4*p2*(ff31+&
ff34)**1.5)+((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/(2*p2*DSQRT(ff31+ff34))-&
(d1y*((4*d1y*n1*x)/(DSQRT(ff30)*mn1)-(4*d1y*ff31**2*n1*x)/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12tz=&
(4*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(-((d1y*DSQRT(ff31+ff34))/p2)-&
(a1*x)/sq1)*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+ff34+x2/mn1+z2)**3+&
(d1y*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(p2*DSQRT(ff31+&
ff34)*(a12+d1y2+ff34+x2/mn1+z2)**2)-(2*((a1*n1)/sq1-(d1y*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+&
ff34)))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+ff34+x2/mn1+z2)**2-&
(2*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1)*((-4*n1*x*z)/(DSQRT(ff30)*mn1)+(ff31*n1*x*(8*a12*z+&
4*ff31*z))/(ff30**1.5*mn1)))/(a12+d1y2+ff34+x2/mn1+z2)**2+(2*((d1y*((-2*n1*x)/mn1-&
(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(4*p2*(ff31+&
ff34)**1.5)-(d1y*((-4*n1*x*z)/(DSQRT(ff30)*mn1)+(ff31*n1*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+&
ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12x=&
(-2*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(2*(-(a1/sq1)-(d1y*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+&
ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12xx=&
(4*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))**2*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1))/(a12+&
d1y2+ff34+x2/mn1+z2)**3-(2*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1)*(2/mn1+(2*ff31)/(DSQRT(ff30)*mn1)+&
(4*x2)/(DSQRT(ff30)*mn1**2)-(4*ff31**2*x2)/(ff30**1.5*mn1**2)))/(a12+d1y2+ff34+&
x2/mn1+z2)**2-(4*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(-(a1/sq1)-(d1y*((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+ff34+x2/mn1+&
z2)**2+(2*((d1y*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))**2)/(4*p2*(ff31+ff34)**1.5)-&
(d1y*(2/mn1+(2*ff31)/(DSQRT(ff30)*mn1)+(4*x2)/(DSQRT(ff30)*mn1**2)-(4*ff31**2*x2)/(ff30**1.5*mn1**2)))/(2*p2*DSQRT(ff31+&
ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12xy=&
(4*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(-&
((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1))/(a12+d1y2+ff34+x2/mn1+z2)**3-(2*(-(d1y*(-&
2*d1y-(2*d1y*ff31)/DSQRT(ff30)))/(2*p2*DSQRT(ff31+ff34))+DSQRT(ff31+ff34)/p2)*((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1)))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*((-4*d1y*x)/(DSQRT(ff30)*mn1)+&
(4*d1y*ff31**2*x)/(ff30**1.5*mn1))*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1))/(a12+&
d1y2+ff34+x2/mn1+z2)**2-(2*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*(-(a1/sq1)-(d1y*((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+ff34+x2/mn1+&
z2)**2+(2*((d1y*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(4*p2*(ff31+&
ff34)**1.5)+((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))/(2*p2*DSQRT(ff31+ff34))-(d1y*((-&
4*d1y*x)/(DSQRT(ff30)*mn1)+(4*d1y*ff31**2*x)/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12xz=&
(4*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1)*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+ff34+x2/mn1+z2)**3+(d1y*((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(p2*DSQRT(ff31+&
ff34)*(a12+d1y2+ff34+x2/mn1+z2)**2)-(2*(-(a1/sq1)-(d1y*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1)))/(2*p2*DSQRT(ff31+&
ff34)))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+ff34+x2/mn1+z2)**2-&
(2*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1)*((4*x*z)/(DSQRT(ff30)*mn1)-(ff31*x*(8*a12*z+&
4*ff31*z))/(ff30**1.5*mn1)))/(a12+d1y2+ff34+x2/mn1+z2)**2+(2*((d1y*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(4*p2*(ff31+ff34)**1.5)-(d1y*((4*x*z)/(DSQRT(ff30)*mn1)-&
(ff31*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12y=&
(-2*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1))/(a12+&
d1y2+ff34+x2/mn1+z2)**2+(2*(-(d1y*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30)))/(2*p2*DSQRT(ff31+&
ff34))+DSQRT(ff31+ff34)/p2))/(a12+d1y2+ff34+x2/mn1+z2)

l12yy=&
(4*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))**2*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1))/(a12+&
d1y2+ff34+x2/mn1+z2)**3-(4*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*(-(d1y*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30)))/(2*p2*DSQRT(ff31+&
ff34))+DSQRT(ff31+ff34)/p2))/(a12+d1y2+ff34+x2/mn1+z2)**2-(2*(2+(4*d1y2)/DSQRT(ff30)+&
(2*ff31)/DSQRT(ff30)-(4*d1y2*ff31**2)/ff30**1.5)*(-((d1y*DSQRT(ff31+ff34))/p2)-&
(a1*x)/sq1))/(a12+d1y2+ff34+x2/mn1+z2)**2+(2*((d1y*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))**2)/(4*p2*(ff31+&
ff34)**1.5)+(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))/(p2*DSQRT(ff31+ff34))-(d1y*(2+(4*d1y2)/DSQRT(ff30)+&
(2*ff31)/DSQRT(ff30)-(4*d1y2*ff31**2)/ff30**1.5))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12yz=&
(4*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1)*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+ff34+x2/mn1+z2)**3+(d1y*(-2*d1y-&
(2*d1y*ff31)/DSQRT(ff30))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(p2*DSQRT(ff31+&
ff34)*(a12+d1y2+ff34+x2/mn1+z2)**2)-(2*(-(d1y*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30)))/(2*p2*DSQRT(ff31+&
ff34))+DSQRT(ff31+ff34)/p2)*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+d1y2+&
ff34+x2/mn1+z2)**2-(2*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1)*((-4*d1y*z)/DSQRT(ff30)+&
(d1y*ff31*(8*a12*z+4*ff31*z))/ff30**1.5))/(a12+d1y2+ff34+x2/mn1+z2)**2+(2*((d1y*(-&
2*d1y-(2*d1y*ff31)/DSQRT(ff30))*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(4*p2*(ff31+&
ff34)**1.5)+(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))/(2*p2*DSQRT(ff31+ff34))-(d1y*((-&
4*d1y*z)/DSQRT(ff30)+(d1y*ff31*(8*a12*z+4*ff31*z))/ff30**1.5))/(2*p2*DSQRT(ff31+ff34))))/(a12+d1y2+ff34+x2/mn1+z2)

l12z=&
(-2*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1)*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(a12+&
d1y2+ff34+x2/mn1+z2)**2-(d1y*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(p2*DSQRT(ff31+ff34)*(a12+d1y2+ff34+x2/mn1+z2))

l12zz=&
(4*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1)*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))**2)/(a12+&
d1y2+ff34+x2/mn1+z2)**3+(p2*d1y*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))**2)/(DSQRT(ff31+&
ff34)*(a12+d1y2+ff34+x2/mn1+z2)**2)+(d1y*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30)))**2)/(2*p2*(ff31+&
ff34)**1.5*(a12+d1y2+ff34+x2/mn1+z2))-(2*(-((d1y*DSQRT(ff31+ff34))/p2)-(a1*x)/sq1)*(2-&
(8*a12*z+4*ff31*z)**2/(4*ff30**1.5)+(8*a12+4*ff31+8*z2)/(2*DSQRT(ff30))))/(a12+&
d1y2+ff34+x2/mn1+z2)**2-(d1y*(2-(8*a12*z+4*ff31*z)**2/(4*ff30**1.5)+(8*a12+4*ff31+&
8*z2)/(2*DSQRT(ff30))))/(p2*DSQRT(ff31+ff34)*(a12+d1y2+ff34+x2/mn1+z2))


l13=&
(p2*z)/DSQRT(ff31+ff34)

l13t=&
-((((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z)/(p2*(ff31+ff34)**1.5))

l13tt=&
(3*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))**2*z)/(2*p2*(ff31+ff34)**2.5)-&
(((2*n12)/mn1+(2*ff31*n12)/(DSQRT(ff30)*mn1)+(4*n12*x2)/(DSQRT(ff30)*mn1**2)-&
(4*ff31**2*n12*x2)/(ff30**1.5*mn1**2))*z)/(p2*(ff31+ff34)**1.5)

l13tx=&
(3*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z)/(2*p2*(ff31+&
ff34)**2.5)-(((-2*n1)/mn1-(2*ff31*n1)/(DSQRT(ff30)*mn1)-(4*n1*x2)/(DSQRT(ff30)*mn1**2)+&
(4*ff31**2*n1*x2)/(ff30**1.5*mn1**2))*z)/(p2*(ff31+ff34)**1.5)

l13ty=&
(3*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z)/(2*p2*(ff31+&
ff34)**2.5)-(((4*d1y*n1*x)/(DSQRT(ff30)*mn1)-(4*d1y*ff31**2*n1*x)/(ff30**1.5*mn1))*z)/(p2*(ff31+ff34)**1.5)

l13tz=&
-(((-2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))/(p2*(ff31+ff34)**1.5))+(3*((-&
2*n1*x)/mn1-(2*ff31*n1*x)/(DSQRT(ff30)*mn1))*z*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(2*p2*(ff31+&
ff34)**2.5)-(z*((-4*n1*x*z)/(DSQRT(ff30)*mn1)+(ff31*n1*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(p2*(ff31+ff34)**1.5)

l13x=&
-((((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z)/(p2*(ff31+ff34)**1.5))

l13xx=&
(3*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))**2*z)/(2*p2*(ff31+ff34)**2.5)-((2/mn1+&
(2*ff31)/(DSQRT(ff30)*mn1)+(4*x2)/(DSQRT(ff30)*mn1**2)-(4*ff31**2*x2)/(ff30**1.5*mn1**2))*z)/(p2*(ff31+ff34)**1.5)

l13xy=&
(3*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))*z)/(2*p2*(ff31+&
ff34)**2.5)-(((-4*d1y*x)/(DSQRT(ff30)*mn1)+(4*d1y*ff31**2*x)/(ff30**1.5*mn1))*z)/(p2*(ff31+ff34)**1.5)

l13xz=&
-(((2*x)/mn1+(2*ff31*x)/(DSQRT(ff30)*mn1))/(p2*(ff31+ff34)**1.5))+(3*((2*x)/mn1+&
(2*ff31*x)/(DSQRT(ff30)*mn1))*z*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(2*p2*(ff31+&
ff34)**2.5)-(z*((4*x*z)/(DSQRT(ff30)*mn1)-(ff31*x*(8*a12*z+4*ff31*z))/(ff30**1.5*mn1)))/(p2*(ff31+ff34)**1.5)

l13y=&
-(((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z)/(p2*(ff31+ff34)**1.5))

l13yy=&
(3*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))**2*z)/(2*p2*(ff31+ff34)**2.5)-((2+(4*d1y2)/DSQRT(ff30)+&
(2*ff31)/DSQRT(ff30)-(4*d1y2*ff31**2)/ff30**1.5)*z)/(p2*(ff31+ff34)**1.5)

l13yz=&
-((-2*d1y-(2*d1y*ff31)/DSQRT(ff30))/(p2*(ff31+ff34)**1.5))+(3*(-2*d1y-(2*d1y*ff31)/DSQRT(ff30))*z*(2*z+&
(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(2*p2*(ff31+ff34)**2.5)-(z*((-4*d1y*z)/DSQRT(ff30)+&
(d1y*ff31*(8*a12*z+4*ff31*z))/ff30**1.5))/(p2*(ff31+ff34)**1.5)

l13z=&
p2/DSQRT(ff31+ff34)-(z*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(p2*(ff31+ff34)**1.5)

l13zz=&
-((p2*(2*z+(8*a12*z+4*ff31*z)/(2*DSQRT(ff30))))/(ff31+ff34)**1.5)+(3*z*(2*z+(8*a12*z+&
4*ff31*z)/(2*DSQRT(ff30)))**2)/(2*p2*(ff31+ff34)**2.5)-(z*(2-(8*a12*z+4*ff31*z)**2/(4*ff30**1.5)+&
(8*a12+4*ff31+8*z2)/(2*DSQRT(ff30))))/(p2*(ff31+ff34)**1.5)


l21=&
(2*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21t=&
(-2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*((DSQRT(ff33+ff35)*n2)/(p2*sq2)+(x*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21tt=&
(4*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))**2*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**3-(2*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2))*((2*n22)/mn2+&
(2*ff33*n22)/(DSQRT(ff32)*mn2)+(4*n22*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*n22*x2)/(ff32**1.5*mn2**2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**2-(4*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*((DSQRT(ff33+&
ff35)*n2)/(p2*sq2)+(x*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)**2+(2*((n2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(p2*DSQRT(ff33+&
ff35)*sq2)-(x*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))**2)/(4*p2*(ff33+ff35)**1.5*sq2)+&
(x*((2*n22)/mn2+(2*ff33*n22)/(DSQRT(ff32)*mn2)+(4*n22*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*n22*x2)/(ff32**1.5*mn2**2)))/(2*p2*DSQRT(ff33+ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21tx=&
(4*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(a2*d2y+&
(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)**3-(2*(a2*d2y+(DSQRT(ff33+&
ff35)*x)/(p2*sq2))*((2*n2)/mn2+(2*ff33*n2)/(DSQRT(ff32)*mn2)+(4*n2*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*n2*x2)/(ff32**1.5*mn2**2)))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(DSQRT(ff33+ff35)/(p2*sq2)+(x*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((DSQRT(ff33+&
ff35)*n2)/(p2*sq2)+(x*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)**2+(2*((n2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)+((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/(2*p2*DSQRT(ff33+ff35)*sq2)-&
(x*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(4*p2*(ff33+&
ff35)**1.5*sq2)+(x*((2*n2)/mn2+(2*ff33*n2)/(DSQRT(ff32)*mn2)+(4*n2*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*n2*x2)/(ff32**1.5*mn2**2)))/(2*p2*DSQRT(ff33+ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21ty=&
(4*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(a2*d2y+&
(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)**3-(2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(a2+&
((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*x)/(2*p2*DSQRT(ff33+ff35)*sq2)))/(a22+d2y2+ff35+&
x2/mn2+z2)**2-(2*((4*d2y*n2*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*n2*x)/(ff32**1.5*mn2))*(a2*d2y+&
(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((DSQRT(ff33+&
ff35)*n2)/(p2*sq2)+(x*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)**2+(2*(((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*n2)/(2*p2*DSQRT(ff33+&
ff35)*sq2)-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*x*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(4*p2*(ff33+&
ff35)**1.5*sq2)+(x*((4*d2y*n2*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*n2*x)/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21tz=&
(4*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2))*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+d2y2+ff35+x2/mn2+z2)**3-(x*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(p2*DSQRT(ff33+&
ff35)*sq2*(a22+d2y2+ff35+x2/mn2+z2)**2)-(2*((DSQRT(ff33+ff35)*n2)/(p2*sq2)+(x*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+ff35)*sq2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+&
d2y2+ff35+x2/mn2+z2)**2-(2*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2))*((4*n2*x*z)/(DSQRT(ff32)*mn2)-&
(ff33*n2*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(a22+d2y2+ff35+x2/mn2+z2)**2+(2*((n2*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(2*p2*DSQRT(ff33+ff35)*sq2)-(x*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(4*p2*(ff33+&
ff35)**1.5*sq2)+(x*((4*n2*x*z)/(DSQRT(ff32)*mn2)-(ff33*n2*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21x=&
(-2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*(DSQRT(ff33+ff35)/(p2*sq2)+(x*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21xx=&
(4*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))**2*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**3-(2*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2))*(2/mn2+(2*ff33)/(DSQRT(ff32)*mn2)+&
(4*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*x2)/(ff32**1.5*mn2**2)))/(a22+d2y2+ff35+&
x2/mn2+z2)**2-(4*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*(DSQRT(ff33+ff35)/(p2*sq2)+&
(x*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+ff35)*sq2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*(((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))/(p2*DSQRT(ff33+&
ff35)*sq2)-(x*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))**2)/(4*p2*(ff33+ff35)**1.5*sq2)+&
(x*(2/mn2+(2*ff33)/(DSQRT(ff32)*mn2)+(4*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*x2)/(ff32**1.5*mn2**2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21xy=&
(4*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*(a2*d2y+&
(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)**3-(2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*(a2+&
((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*x)/(2*p2*DSQRT(ff33+ff35)*sq2)))/(a22+d2y2+ff35+&
x2/mn2+z2)**2-(2*((4*d2y*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*x)/(ff32**1.5*mn2))*(a2*d2y+&
(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*(DSQRT(ff33+&
ff35)/(p2*sq2)+(x*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+ff35)*sq2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*((2*d2y+(2*d2y*ff33)/DSQRT(ff32))/(2*p2*DSQRT(ff33+ff35)*sq2)-&
((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*x*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(4*p2*(ff33+&
ff35)**1.5*sq2)+(x*((4*d2y*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*x)/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21xz=&
(4*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2))*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+d2y2+ff35+x2/mn2+z2)**3-(x*((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(p2*DSQRT(ff33+&
ff35)*sq2*(a22+d2y2+ff35+x2/mn2+z2)**2)-(2*(DSQRT(ff33+ff35)/(p2*sq2)+(x*((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+ff35)*sq2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+&
d2y2+ff35+x2/mn2+z2)**2-(2*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2))*((4*x*z)/(DSQRT(ff32)*mn2)-&
(ff33*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(a22+d2y2+ff35+x2/mn2+z2)**2+(2*((2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))/(2*p2*DSQRT(ff33+ff35)*sq2)-(x*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(4*p2*(ff33+ff35)**1.5*sq2)+(x*((4*x*z)/(DSQRT(ff32)*mn2)-&
(ff33*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21y=&
(-2*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*(a2+((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*x)/(2*p2*DSQRT(ff33+ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21yy=&
(4*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))**2*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**3-(4*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*(a2+((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*x)/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*(2+(4*d2y2)/DSQRT(ff32)+(2*ff33)/DSQRT(ff32)-&
(4*d2y2*ff33**2)/ff32**1.5)*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2)))/(a22+d2y2+ff35+&
x2/mn2+z2)**2+(2*(-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))**2*x)/(4*p2*(ff33+ff35)**1.5*sq2)+&
((2+(4*d2y2)/DSQRT(ff32)+(2*ff33)/DSQRT(ff32)-(4*d2y2*ff33**2)/ff32**1.5)*x)/(2*p2*DSQRT(ff33+&
ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21yz=&
(4*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2))*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+d2y2+ff35+x2/mn2+z2)**3-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*x*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(p2*DSQRT(ff33+ff35)*sq2*(a22+d2y2+ff35+x2/mn2+&
z2)**2)-(2*(a2+((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*x)/(2*p2*DSQRT(ff33+ff35)*sq2))*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*(a2*d2y+(DSQRT(ff33+&
ff35)*x)/(p2*sq2))*((4*d2y*z)/DSQRT(ff32)-(d2y*ff33*(8*a22*z+4*ff33*z))/ff32**1.5))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*(-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*x*(2*z+(8*a22*z+&
4*ff33*z)/(2*DSQRT(ff32))))/(4*p2*(ff33+ff35)**1.5*sq2)+(x*((4*d2y*z)/DSQRT(ff32)-&
(d2y*ff33*(8*a22*z+4*ff33*z))/ff32**1.5))/(2*p2*DSQRT(ff33+ff35)*sq2)))/(a22+d2y2+ff35+x2/mn2+z2)

l21z=&
(-2*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(x*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(p2*DSQRT(ff33+ff35)*sq2*(a22+d2y2+ff35+x2/mn2+z2))

l21zz=&
(4*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))**2)/(a22+&
d2y2+ff35+x2/mn2+z2)**3-(p2*x*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))**2)/(DSQRT(ff33+&
ff35)*sq2*(a22+d2y2+ff35+x2/mn2+z2)**2)-(x*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))**2)/(2*p2*(ff33+&
ff35)**1.5*sq2*(a22+d2y2+ff35+x2/mn2+z2))-(2*(a2*d2y+(DSQRT(ff33+ff35)*x)/(p2*sq2))*(2-&
(8*a22*z+4*ff33*z)**2/(4*ff32**1.5)+(8*a22+4*ff33+8*z2)/(2*DSQRT(ff32))))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(x*(2-(8*a22*z+4*ff33*z)**2/(4*ff32**1.5)+(8*a22+4*ff33+&
8*z2)/(2*DSQRT(ff32))))/(p2*DSQRT(ff33+ff35)*sq2*(a22+d2y2+ff35+x2/mn2+z2))


l22=&
(2*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2))/(a22+d2y2+ff35+x2/mn2+z2)

l22t=&
(-2*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*(-((a2*n2)/sq2)+(d2y*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22tt=&
(4*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))**2*((d2y*DSQRT(ff33+ff35))/p2-&
(a2*x)/sq2))/(a22+d2y2+ff35+x2/mn2+z2)**3-(2*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*((2*n22)/mn2+&
(2*ff33*n22)/(DSQRT(ff32)*mn2)+(4*n22*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*n22*x2)/(ff32**1.5*mn2**2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**2-(4*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(-((a2*n2)/sq2)+&
(d2y*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+ff35))))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*(-(d2y*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))**2)/(4*p2*(ff33+&
ff35)**1.5)+(d2y*((2*n22)/mn2+(2*ff33*n22)/(DSQRT(ff32)*mn2)+(4*n22*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*n22*x2)/(ff32**1.5*mn2**2)))/(2*p2*DSQRT(ff33+ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22tx=&
(4*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*((d2y*DSQRT(ff33+&
ff35))/p2-(a2*x)/sq2))/(a22+d2y2+ff35+x2/mn2+z2)**3-(2*((d2y*DSQRT(ff33+ff35))/p2-&
(a2*x)/sq2)*((2*n2)/mn2+(2*ff33*n2)/(DSQRT(ff32)*mn2)+(4*n2*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*n2*x2)/(ff32**1.5*mn2**2)))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(-(a2/sq2)+(d2y*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35))))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*(-&
((a2*n2)/sq2)+(d2y*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35))))/(a22+d2y2+ff35+x2/mn2+z2)**2+(2*(-(d2y*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(4*p2*(ff33+ff35)**1.5)+(d2y*((2*n2)/mn2+(2*ff33*n2)/(DSQRT(ff32)*mn2)+&
(4*n2*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*n2*x2)/(ff32**1.5*mn2**2)))/(2*p2*DSQRT(ff33+ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22ty=&
(4*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*((d2y*DSQRT(ff33+&
ff35))/p2-(a2*x)/sq2))/(a22+d2y2+ff35+x2/mn2+z2)**3-(2*((d2y*(2*d2y+(2*d2y*ff33)/DSQRT(ff32)))/(2*p2*DSQRT(ff33+&
ff35))+DSQRT(ff33+ff35)/p2)*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(a22+&
d2y2+ff35+x2/mn2+z2)**2-(2*((4*d2y*n2*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*n2*x)/(ff32**1.5*mn2))*((d2y*DSQRT(ff33+&
ff35))/p2-(a2*x)/sq2))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*(-&
((a2*n2)/sq2)+(d2y*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35))))/(a22+d2y2+ff35+x2/mn2+z2)**2+(2*(-(d2y*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(4*p2*(ff33+ff35)**1.5)+((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/(2*p2*DSQRT(ff33+&
ff35))+(d2y*((4*d2y*n2*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*n2*x)/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+&
ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22tz=&
(4*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+d2y2+ff35+x2/mn2+z2)**3-(d2y*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(p2*DSQRT(ff33+&
ff35)*(a22+d2y2+ff35+x2/mn2+z2)**2)-(2*(-((a2*n2)/sq2)+(d2y*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35)))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+d2y2+ff35+x2/mn2+z2)**2-&
(2*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*((4*n2*x*z)/(DSQRT(ff32)*mn2)-(ff33*n2*x*(8*a22*z+&
4*ff33*z))/(ff32**1.5*mn2)))/(a22+d2y2+ff35+x2/mn2+z2)**2+(2*(-(d2y*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(4*p2*(ff33+&
ff35)**1.5)+(d2y*((4*n2*x*z)/(DSQRT(ff32)*mn2)-(ff33*n2*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+&
ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22x=&
(-2*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*(-(a2/sq2)+(d2y*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22xx=&
(4*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))**2*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2))/(a22+&
d2y2+ff35+x2/mn2+z2)**3-(2*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*(2/mn2+(2*ff33)/(DSQRT(ff32)*mn2)+&
(4*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*x2)/(ff32**1.5*mn2**2)))/(a22+d2y2+ff35+&
x2/mn2+z2)**2-(4*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*(-(a2/sq2)+(d2y*((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+ff35))))/(a22+d2y2+ff35+x2/mn2+&
z2)**2+(2*(-(d2y*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))**2)/(4*p2*(ff33+ff35)**1.5)+&
(d2y*(2/mn2+(2*ff33)/(DSQRT(ff32)*mn2)+(4*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*x2)/(ff32**1.5*mn2**2)))/(2*p2*DSQRT(ff33+&
ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22xy=&
(4*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((d2y*DSQRT(ff33+&
ff35))/p2-(a2*x)/sq2))/(a22+d2y2+ff35+x2/mn2+z2)**3-(2*((d2y*(2*d2y+(2*d2y*ff33)/DSQRT(ff32)))/(2*p2*DSQRT(ff33+&
ff35))+DSQRT(ff33+ff35)/p2)*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(a22+d2y2+&
ff35+x2/mn2+z2)**2-(2*((4*d2y*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*x)/(ff32**1.5*mn2))*((d2y*DSQRT(ff33+&
ff35))/p2-(a2*x)/sq2))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*(-&
(a2/sq2)+(d2y*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+ff35))))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*(-(d2y*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(4*p2*(ff33+&
ff35)**1.5)+((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))/(2*p2*DSQRT(ff33+ff35))+(d2y*((4*d2y*x)/(DSQRT(ff32)*mn2)-&
(4*d2y*ff33**2*x)/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22xz=&
(4*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+d2y2+ff35+x2/mn2+z2)**3-(d2y*((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(p2*DSQRT(ff33+&
ff35)*(a22+d2y2+ff35+x2/mn2+z2)**2)-(2*(-(a2/sq2)+(d2y*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2)))/(2*p2*DSQRT(ff33+&
ff35)))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+d2y2+ff35+x2/mn2+z2)**2-&
(2*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*((4*x*z)/(DSQRT(ff32)*mn2)-(ff33*x*(8*a22*z+&
4*ff33*z))/(ff32**1.5*mn2)))/(a22+d2y2+ff35+x2/mn2+z2)**2+(2*(-(d2y*((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2))*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(4*p2*(ff33+&
ff35)**1.5)+(d2y*((4*x*z)/(DSQRT(ff32)*mn2)-(ff33*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(2*p2*DSQRT(ff33+&
ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22y=&
(-2*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*((d2y*(2*d2y+(2*d2y*ff33)/DSQRT(ff32)))/(2*p2*DSQRT(ff33+&
ff35))+DSQRT(ff33+ff35)/p2))/(a22+d2y2+ff35+x2/mn2+z2)

l22yy=&
(4*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))**2*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2))/(a22+&
d2y2+ff35+x2/mn2+z2)**3-(4*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((d2y*(2*d2y+(2*d2y*ff33)/DSQRT(ff32)))/(2*p2*DSQRT(ff33+&
ff35))+DSQRT(ff33+ff35)/p2))/(a22+d2y2+ff35+x2/mn2+z2)**2-(2*(2+(4*d2y2)/DSQRT(ff32)+&
(2*ff33)/DSQRT(ff32)-(4*d2y2*ff33**2)/ff32**1.5)*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(2*(-(d2y*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))**2)/(4*p2*(ff33+&
ff35)**1.5)+(2*d2y+(2*d2y*ff33)/DSQRT(ff32))/(p2*DSQRT(ff33+ff35))+(d2y*(2+(4*d2y2)/DSQRT(ff32)+&
(2*ff33)/DSQRT(ff32)-(4*d2y2*ff33**2)/ff32**1.5))/(2*p2*DSQRT(ff33+ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22yz=&
(4*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+d2y2+ff35+x2/mn2+z2)**3-(d2y*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(p2*DSQRT(ff33+ff35)*(a22+d2y2+ff35+x2/mn2+&
z2)**2)-(2*((d2y*(2*d2y+(2*d2y*ff33)/DSQRT(ff32)))/(2*p2*DSQRT(ff33+ff35))+DSQRT(ff33+&
ff35)/p2)*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+d2y2+ff35+x2/mn2+z2)**2-&
(2*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*((4*d2y*z)/DSQRT(ff32)-(d2y*ff33*(8*a22*z+&
4*ff33*z))/ff32**1.5))/(a22+d2y2+ff35+x2/mn2+z2)**2+(2*(-(d2y*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(4*p2*(ff33+ff35)**1.5)+(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))/(2*p2*DSQRT(ff33+&
ff35))+(d2y*((4*d2y*z)/DSQRT(ff32)-(d2y*ff33*(8*a22*z+4*ff33*z))/ff32**1.5))/(2*p2*DSQRT(ff33+&
ff35))))/(a22+d2y2+ff35+x2/mn2+z2)

l22z=&
(-2*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(d2y*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(p2*DSQRT(ff33+ff35)*(a22+d2y2+ff35+x2/mn2+z2))

l22zz=&
(4*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))**2)/(a22+&
d2y2+ff35+x2/mn2+z2)**3-(p2*d2y*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))**2)/(DSQRT(ff33+&
ff35)*(a22+d2y2+ff35+x2/mn2+z2)**2)-(d2y*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32)))**2)/(2*p2*(ff33+&
ff35)**1.5*(a22+d2y2+ff35+x2/mn2+z2))-(2*((d2y*DSQRT(ff33+ff35))/p2-(a2*x)/sq2)*(2-&
(8*a22*z+4*ff33*z)**2/(4*ff32**1.5)+(8*a22+4*ff33+8*z2)/(2*DSQRT(ff32))))/(a22+&
d2y2+ff35+x2/mn2+z2)**2+(d2y*(2-(8*a22*z+4*ff33*z)**2/(4*ff32**1.5)+(8*a22+4*ff33+&
8*z2)/(2*DSQRT(ff32))))/(p2*DSQRT(ff33+ff35)*(a22+d2y2+ff35+x2/mn2+z2))


l23=&
(p2*z)/DSQRT(ff33+ff35)

l23t=&
-((((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z)/(p2*(ff33+ff35)**1.5))

l23tt=&
(3*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))**2*z)/(2*p2*(ff33+ff35)**2.5)-&
(((2*n22)/mn2+(2*ff33*n22)/(DSQRT(ff32)*mn2)+(4*n22*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*n22*x2)/(ff32**1.5*mn2**2))*z)/(p2*(ff33+ff35)**1.5)

l23tx=&
(3*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z)/(2*p2*(ff33+&
ff35)**2.5)-(((2*n2)/mn2+(2*ff33*n2)/(DSQRT(ff32)*mn2)+(4*n2*x2)/(DSQRT(ff32)*mn2**2)-&
(4*ff33**2*n2*x2)/(ff32**1.5*mn2**2))*z)/(p2*(ff33+ff35)**1.5)

l23ty=&
(3*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z)/(2*p2*(ff33+&
ff35)**2.5)-(((4*d2y*n2*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*n2*x)/(ff32**1.5*mn2))*z)/(p2*(ff33+ff35)**1.5)

l23tz=&
-(((2*n2*x)/mn2+(2*ff33*n2*x)/(DSQRT(ff32)*mn2))/(p2*(ff33+ff35)**1.5))+(3*((2*n2*x)/mn2+&
(2*ff33*n2*x)/(DSQRT(ff32)*mn2))*z*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(2*p2*(ff33+&
ff35)**2.5)-(z*((4*n2*x*z)/(DSQRT(ff32)*mn2)-(ff33*n2*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(p2*(ff33+ff35)**1.5)

l23x=&
-((((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z)/(p2*(ff33+ff35)**1.5))

l23xx=&
(3*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))**2*z)/(2*p2*(ff33+ff35)**2.5)-((2/mn2+&
(2*ff33)/(DSQRT(ff32)*mn2)+(4*x2)/(DSQRT(ff32)*mn2**2)-(4*ff33**2*x2)/(ff32**1.5*mn2**2))*z)/(p2*(ff33+ff35)**1.5)

l23xy=&
(3*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))*z)/(2*p2*(ff33+&
ff35)**2.5)-(((4*d2y*x)/(DSQRT(ff32)*mn2)-(4*d2y*ff33**2*x)/(ff32**1.5*mn2))*z)/(p2*(ff33+ff35)**1.5)

l23xz=&
-(((2*x)/mn2+(2*ff33*x)/(DSQRT(ff32)*mn2))/(p2*(ff33+ff35)**1.5))+(3*((2*x)/mn2+&
(2*ff33*x)/(DSQRT(ff32)*mn2))*z*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(2*p2*(ff33+&
ff35)**2.5)-(z*((4*x*z)/(DSQRT(ff32)*mn2)-(ff33*x*(8*a22*z+4*ff33*z))/(ff32**1.5*mn2)))/(p2*(ff33+ff35)**1.5)

l23y=&
-(((2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z)/(p2*(ff33+ff35)**1.5))

l23yy=&
(3*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))**2*z)/(2*p2*(ff33+ff35)**2.5)-((2+(4*d2y2)/DSQRT(ff32)+&
(2*ff33)/DSQRT(ff32)-(4*d2y2*ff33**2)/ff32**1.5)*z)/(p2*(ff33+ff35)**1.5)

l23yz=&
-((2*d2y+(2*d2y*ff33)/DSQRT(ff32))/(p2*(ff33+ff35)**1.5))+(3*(2*d2y+(2*d2y*ff33)/DSQRT(ff32))*z*(2*z+&
(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(2*p2*(ff33+ff35)**2.5)-(z*((4*d2y*z)/DSQRT(ff32)-&
(d2y*ff33*(8*a22*z+4*ff33*z))/ff32**1.5))/(p2*(ff33+ff35)**1.5)

l23z=&
p2/DSQRT(ff33+ff35)-(z*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(p2*(ff33+ff35)**1.5)

l23zz=&
-((p2*(2*z+(8*a22*z+4*ff33*z)/(2*DSQRT(ff32))))/(ff33+ff35)**1.5)+(3*z*(2*z+(8*a22*z+&
4*ff33*z)/(2*DSQRT(ff32)))**2)/(2*p2*(ff33+ff35)**2.5)-(z*(2-(8*a22*z+4*ff33*z)**2/(4*ff32**1.5)+&
(8*a22+4*ff33+8*z2)/(2*DSQRT(ff32))))/(p2*(ff33+ff35)**1.5)


l11n1=l11-n1
l21n2=l21+n2

l11n12=l11n1*l11n1
l21n22=l21n2*l21n2

l112=l11*l11
l122=l12*l12
l132=l13*l13
l212=l21*l21
l222=l22*l22
l232=l23*l23

!-----------------------------------------------------------------------------------------------------------------------------

f1=l11*n1
f2=l11x*n1
f3=l11y*n1
f4=l11z*n1

f5=l12*n1
f6=l12x*n1
f7=l12y*n1
f8=l12z*n1

f9=l13*n1
f10=l13x*n1
f11=l13y*n1
f12=l13z*n1

f13=l21*n1
f14=l21x*n1
f15=l21y*n1

f16=l22*n1
f17=l22x*n1
f18=l22xx*n1
f19=l22xy*n1
f20=l22xz*n1
f21=l22y*n1
f22=l22yy*n1
f23=l22yz*n1
f24=l22z*n1

f25=l23*n1
f26=l23x*n1
f27=l23y*n1


f28=l21*n2
f29=l21x*n2
f30=l21xx*n2
f31=l21xy*n2
f32=l21xz*n2
f33=l21y*n2
f34=l21yy*n2
f35=l21yz*n2
f36=l21z*n2

f37=l22*n2
f38=l22x*n2
f39=l22y*n2
f40=l22z*n2

f41=l23*n2
f42=l23x*n2
f43=l23y*n2
f44=l23z*n2


f45=H1*l11x
f46=H1*l11xx
f47=H1*l11xy
f48=H1*l11xz
f49=H1*l11y
f50=H1*l11yy
f51=H1*l11yz
f52=H1*l11z
f53=H1*l11zz

f54=H1*l12
f55=H1*l12x
f56=H1*l12xx
f57=H1*l12xy
f58=H1*l12xz
f59=H1*l12y
f60=H1*l12yy
f61=H1*l12yz
f62=H1*l12z
f63=H1*l12zz

f64=H1*l13
f65=H1*l13x
f66=H1*l13xx
f67=H1*l13xy
f68=H1*l13xz
f69=H1*l13y
f70=H1*l13yy
f71=H1*l13yz
f72=H1*l13z


f73=H2*l11x
f74=H2*l11xx
f75=H2*l11xy
f76=H2*l11xz
f77=H2*l11y
f78=H2*l11yy
f79=H2*l11yz
f80=H2*l11z

f81=H2*l12
f82=H2*l12x
f83=H2*l12xx
f84=H2*l12xy
f85=H2*l12xz
f86=H2*l12y
f87=H2*l12yy
f88=H2*l12yz
f89=H2*l12z

f90=H2*l13
f91=H2*l13x
f92=H2*l13y

f93=H2*l21x
f94=H2*l21xx
f95=H2*l21xy
f96=H2*l21xz
f97=H2*l21y
f98=H2*l21yy
f99=H2*l21yz
f100=H2*l21z
f101=H2*l21zz

f102=H2*l22
f103=H2*l22x
f104=H2*l22xx
f105=H2*l22xy
f106=H2*l22xz
f107=H2*l22y
f108=H2*l22yy
f109=H2*l22yz
f110=H2*l22z

f111=H2*l23
f112=H2*l23x
f113=H2*l23xx
f114=H2*l23xy
f115=H2*l23xz
f116=H2*l23y
f117=H2*l23yy
f118=H2*l23yz
f119=H2*l23z


ff1=l11x*l11x
ff2=l12x*l12x
ff3=l13x*l13x
ff4=l21x*l21x
ff5=l22x*l22x

ff6=l11y*l11y
ff7=l12y*l12y
ff8=l13y*l13y
ff9=l21y*l21y
ff10=l22y*l22y
ff11=l23y*l23y

ff12=2*H2x*l222
ff13=2*H2y*l222
ff14=2*H2z*l222

ff15=2*H1x*l122
ff16=2*H1y*l122
ff17=2*H1z*l122

ff18=l23x*l23x
ff19=l11z*l11z
ff20=l12z*l12z
ff21=l21z*l21z
ff22=l22z*l22z

ff23=2*H1*l122
ff24=2*H1*l132
ff25=2*H2*l132
ff26=2*H2*l222
ff27=2*H2*l232

ff28=(2*H1*l11n12)/mn1
ff29=(2*H2*l21n22)/mn2

END SUBROUTINE VarFuncHl

END MODULE module001FunctionsVarHl
