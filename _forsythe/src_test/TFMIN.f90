REAL FUNCTION F(X)
   REAL X
   F = X * (X * X - 2.0) - 5.0
   RETURN
END
!
!     ИЛЛЮCTPИPУЮЩAЯ ПPOГPAMMA ДЛЯ FMIN
!
EXTERNAL F
REAL F, A, B, Z, TOL, FMIN
A = 0.0
B = 1.0
TOL = 1.0E-05
!
Z = FMIN(A, B, F, TOL)
!
PRINT 1, Z
1 FORMAT('   Z=', F12.5)
STOP
END