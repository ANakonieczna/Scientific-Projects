MODULE module003FunctionBigF

CONTAINS
REAL(8) FUNCTION bigF(G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,k1,k2,Nh,KK,dxNh,dyNh,dxKK,dyKK,dxkappa,dykappa,&
 Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh)
!
IMPLICIT NONE
REAL(8),INTENT(IN) :: G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,k1,k2,Nh,KK,dxNh,dyNh,dxKK,dyKK,dxkappa,dykappa
REAL(8),INTENT(IN) :: Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh

bigF=0.25D0/d*(Nh*(G5*(k1+Ak1)-G3*(k2+Ak2))+ANh*(G5*k1-G3*k2))-(-G7*(k1*(dxNh+AdxNh)+Ak1*dxNh)+G4*(k2*(dyNh+AdyNh)+Ak2*dyNh)&
 -G6*(k2*(dxNh+AdxNh)+Ak2*dxNh+k1*(dyNh+AdyNh)+Ak1*dyNh))/d+0.5D0*KK*Kstar

END FUNCTION bigF

END MODULE module003FunctionBigF
