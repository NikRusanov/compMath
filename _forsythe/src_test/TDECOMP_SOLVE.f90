!  ПPOГPAMMA PEШAET CИCTEMУ ИЗ TPEX ЛИHEЙHЫX
!  УPABHEHИЙ, ИCПOЛЬЗУЯ DECOMP И SOLVE
!
REAL A(3, 3), B(3), WORK(3), COND
INTEGER NDIM/3/, N/3/, IPVT(3)
DATA A/10., -3., 5., &
   -7., 2., -1., &
   0., 6., 5./
DATA B/7., 4., 6./
PRINT 101, ((A(I, J), J = 1, N), B(I), I = 1, N)
!
CALL DECOMP(NDIM, N, A, COND, IPVT, WORK)
!
PRINT 102, COND
CONDP1 = COND + 1.0
IF(CONDP1==COND) PRINT 103
IF(CONDP1==COND) STOP
!
CALL SOLVE(NDIM, N, A, B, IPVT)
!
PRINT 104, (B(I), I = 1, N)
STOP
101 FORMAT(13X, 'A', 14X, 'B', 3(/5X, 3F5.0, 5X, F5.0))
102 FORMAT(5X, 'COND=', E12.5)
103 FORMAT(5X, 'MATPИЦA KЛACCИФИЦИPУETCЯ KAK BЫPOЖДEHHAЯ')
104 FORMAT(5X, 'BEKTOP PEШEHИЯ X', 3(/12X, F10.7))
END

!   B PEЗУЛЬTATE EE BЫПOЛHEHИЯ БУДET ПOЛУЧEHO

!             A              B
!       10.  -7.   0.        7.
!       -3.   2.   6.        4.
!        5.  -1.   5.        6.
!     COND= 0.11207E+02
!     BEKTOP PEШEHИЯ X
!             0.0
!            -1.0000000
!             1.0000000