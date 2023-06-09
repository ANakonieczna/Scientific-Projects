MODULE LUlinsolv

CONTAINS

SUBROUTINE lubksb(a,indx,b)
!~
! IN:  LU decomposition of the matrix A a, indx, the right-hand side vector b
! OUT: vector b with the solution
!~~~
! Solves the set of np linear equations A X = B. 
!~
! a is input, the LU decomposition of the matrix A, determined by the routine ludcmp. 
! indx is input as the permutation vector returned by ludcmp. 
! b(1:n) is input as the right-hand side vector B, and returns with the solution vector X. 
! a and indx are not modified by this routine and can be left in place for successive calls 
! with different right-hand sides b. This routine takes into account the possibility that b will begin with many zero elements, 
! so it is efficient for use in matrix inversion.
!~~~
USE nrtype
USE nrutil, ONLY : assert_eq

IMPLICIT NONE

REAL(SP), DIMENSION(:,:), INTENT(IN) :: a
INTEGER(I4B), DIMENSION(:), INTENT(IN) :: indx
REAL(SP), DIMENSION(:), INTENT(INOUT) :: b

INTEGER(I4B) :: i,n,ii,ll
REAL(SP) :: summ

n=assert_eq(size(a,1),size(a,2),size(indx),'lubksb')

ii=0   ! When ii is set to a positive value, it will become the index of the first nonvanishing element of b. 
       ! We now do the forward substitution, equation (2.3.6). The only new wrinkle is to unscramble the permutation as we go.

do i=1,n
   ll=indx(i)
   summ=b(ll)
   b(ll)=b(i)
   if (ii /= 0) then
      summ=summ-dot_product(a(i,ii:i-1),b(ii:i-1))
   else if (summ /= 0.0) then
      ii=i   ! A nonzero element was encountered, so from now on we will have to do the dot product above.
   end if
   b(i)=summ
end do
do i=n,1,-1   ! Now we do the backsubstitution, equation (2.3.7).
   b(i) = (b(i)-dot_product(a(i,i+1:n),b(i+1:n)))/a(i,i)
end do

END SUBROUTINE lubksb

END MODULE LUlinsolv
