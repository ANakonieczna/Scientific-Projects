MODULE modulepochodne

CONTAINS

!*******************************************************************************************************************
REAL(8) FUNCTION DerivAsymBackward2pts(h,FUNm1,FUN)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUNm1,FUN

DerivAsymBackward2pts=(FUN-FUNm1)/h

END FUNCTION DerivAsymBackward2pts

!*******************************************************************************************************************
REAL(8) FUNCTION DerivAsymForward2pts(h,FUN,FUNp1)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUN,FUNp1

DerivAsymForward2pts=(FUNp1-FUN)/h

END FUNCTION DerivAsymForward2pts

!*******************************************************************************************************************
REAL(8) FUNCTION DerivAsymBackward3pts(h,FUNm2,FUNm1,FUN)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUNm2,FUNm1,FUN

DerivAsymBackward3pts=0.5D0*(FUNm2-4.D0*FUNm1+3.D0*FUN)/h

END FUNCTION DerivAsymBackward3pts

!*******************************************************************************************************************
REAL(8) FUNCTION DerivAsymForward3pts(h,FUN,FUNp1,FUNp2)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUN,FUNp1,FUNp2

DerivAsymForward3pts=-0.5D0*(FUNp2-4.D0*FUNp1+3.D0*FUN)/h

END FUNCTION DerivAsymForward3pts

!*******************************************************************************************************************
REAL(8) FUNCTION DerivSym3pts(h,FUNm1,FUNp1)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUNm1,FUNp1

DerivSym3pts=0.5D0*(FUNp1-FUNm1)/h

END FUNCTION DerivSym3pts

!*******************************************************************************************************************
REAL(8) FUNCTION DerivAsymBackward5pts1(h,FUNm4,FUNm3,FUNm2,FUNm1,FUN)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUNm4,FUNm3,FUNm2,FUNm1,FUN

DerivAsymBackward5pts1=(3.D0*FUNm4-16.D0*FUNm3+36.D0*FUNm2-48.D0*FUNm1+25.D0*FUN)/(12.D0*h)

END FUNCTION DerivAsymBackward5pts1

!*******************************************************************************************************************
REAL(8) FUNCTION DerivAsymForward5pts1(h,FUN,FUNp1,FUNp2,FUNp3,FUNp4)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUN,FUNp1,FUNp2,FUNp3,FUNp4

DerivAsymForward5pts1=-(3.D0*FUNp4-16.D0*FUNp3+36.D0*FUNp2-48.D0*FUNp1+25.D0*FUN)/(12.D0*h)

END FUNCTION DerivAsymForward5pts1

!*******************************************************************************************************************
REAL(8) FUNCTION DerivAsymBackward5pts2(h,FUNm3,FUNm2,FUNm1,FUN,FUNp1)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUNm3,FUNm2,FUNm1,FUN,FUNp1

DerivAsymBackward5pts2=-(FUNm3-6.D0*FUNm2+18.D0*FUNm1-10.D0*FUN-3.D0*FUNp1)/(12.D0*h)

END FUNCTION DerivAsymBackward5pts2

!*******************************************************************************************************************
REAL(8) FUNCTION DerivAsymForward5pts2(h,FUNm1,FUN,FUNp1,FUNp2,FUNp3)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUNm1,FUN,FUNp1,FUNp2,FUNp3

DerivAsymForward5pts2=(FUNp3-6.D0*FUNp2+18.D0*FUNp1-10.D0*FUN-3.D0*FUNm1)/(12.D0*h)

END FUNCTION DerivAsymForward5pts2

!*******************************************************************************************************************
REAL(8) FUNCTION DerivSym5pts(h,FUNm2,FUNm1,FUNp1,FUNp2)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUNm2,FUNm1,FUNp1,FUNp2

DerivSym5pts=(FUNm2-8.D0*FUNm1+8.D0*FUNp1-FUNp2)/(12.D0*h)

END FUNCTION DerivSym5pts

!*******************************************************************************************************************
!*******************************************************************************************************************
REAL(8) FUNCTION DerivAsymForward6pts(h,FUN,FUNp1,FUNp2,FUNp3,FUNp4,FUNp5)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUN,FUNp1,FUNp2,FUNp3,FUNp4,FUNp5

DerivAsymForward6pts=-(137.D0/60.D0*FUN-5.D0*FUNp1+5.D0*FUNp2-10.D0/3.D0*FUNp3+1.25D0*FUNp4-0.2D0*FUNp5)/h

END FUNCTION DerivAsymForward6pts

!*******************************************************************************************************************
REAL(8) FUNCTION DerivAsymForward7pts(h,FUN,FUNp1,FUNp2,FUNp3,FUNp4,FUNp5,FUNp6)
IMPLICIT NONE
REAL(8),INTENT(IN) :: h,FUN,FUNp1,FUNp2,FUNp3,FUNp4,FUNp5,FUNp6

DerivAsymForward7pts=-(49.D0/20.D0*FUN-6.D0*FUNp1+7.5D0*FUNp2-20.D0/3.D0*FUNp3+3.75D0*FUNp4-1.2D0*FUNp5+FUNp6/6.D0)/h

END FUNCTION DerivAsymForward7pts

END MODULE modulepochodne
