!     ИЛЛЮCTPИPУЮЩAЯ ПPOГPAMMA ДЛЯ SEVAL
!
INTEGER N, I
REAL X(10), Y(10), B(10), C(10), D(10)
N = 10
DO I = 1, N
   X(I) = I
   Y(I) = X(I)**3
end do
!
CALL SPLINE (N, X, Y, B, C, D)
!
U = 2.5
S = SEVAL(N, U, X, Y, B, C, D)
!
PRINT 102, U, S
STOP
102 FORMAT(14X, 'U=', F3.1, 5X, 'S=', F10.7)
END