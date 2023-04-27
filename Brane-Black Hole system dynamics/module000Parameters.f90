MODULE module000Parameters

!* zadawane parametry
INTEGER, PARAMETER :: tk=2500   !liczba krokow czasowych
INTEGER, PARAMETER :: d=5   !wymiar czasoprzestrzeni
REAL(8), PARAMETER :: thick=0.1D0   !stosunek grubosci brany do promienia horyzontu zdarzen czarnej dziury, [0.01 ... 1]
REAL(8), PARAMETER :: v=0.1D0  !predkosc poczatkowa, [0.01 ... 0.99]
!*
REAL(8), PARAMETER :: alfa=-1.D0  !dylatonowa stala sprzezenia, [-sqrt(3),-1,0]
REAL(8), PARAMETER :: rM=0.1D0,rP=0.5D0,eta=0.1D0,rI=10.D0*rP
!*

REAL(8), PARAMETER :: hx=0.001D0,hy=0.001D0,dt=0.001D0   !kroki calkowania w kierunkach x, y i t
INTEGER, PARAMETER :: Nx=2001,Ny=2001,NCh=50   !liczba pktow siatki i wielomianow Chebysheva w kierunkach x i y

REAL(8), PARAMETER :: lam=1.D0/(thick*thick*rP*rP*eta*eta)   !lambda z war brane thickness <= rP
REAL(8), PARAMETER :: gam=(2.D0*alfa*alfa*(d-2.D0))/((d-3.D0)*(2.D0*(d-3.D0)+alfa*alfa*(d-2.D0)))   !stala gamma

END MODULE module000Parameters
