MODULE module003FunctionFancyB

CONTAINS
REAL(8) FUNCTION fancyB(G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,&
 a,d,kappa,Kstar,k1,k2,Nh,KK,dxNh,dyNh,dxKK,dyKK,dxkappa,dykappa,&
 Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh)
!
IMPLICIT NONE
REAL(8),INTENT(IN) :: G1,G2,G3,G4,G5,G6,G7,G8,dxG1,dyG1,dxG2,dyG2,a,d,kappa,Kstar,k1,k2,Nh,KK,dxNh,dyNh,dxKK,dyKK,dxkappa,dykappa
REAL(8),INTENT(IN) :: Ak1,Ak2,ANh,AKK,AdxNh,AdyNh,AdxKK,AdyKK,Adxk1,Adyk1,Adxk2,Adyk2,AdxxNh,AdyyNh,AdxyNh

fancyB=-kappa*KK-0.25D0*(KK*KK+2.D0*AKK*KK)+0.5D0/d*(-G7*(k1*k1+2.D0*Ak1*k1)+G4*(k2*k2+2.D0*Ak2*k2)-2.D0*G6*(k1*k2+k1*Ak2+k2*Ak1))

END FUNCTION fancyB

END MODULE module003FunctionFancyB
