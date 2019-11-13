!     ИЛЛЮCTPИPУЮЩAЯ ПPOГPAMMA ДЛЯ SVD
!
REAL A(5, 3), U(5, 3), V(5, 3), SIGMA(5), WORK(5)
INTEGER I, IERR, J, M, N, NM
NM = 5
M = 5
N = 3
DO I = 1, M
   DO 1 J = 1, N
      A(I, J) = I + (J - 1) * M
   1  CONTINUE
end do
!
CALL SVD(NM, M, N, A, SIGMA, .TRUE., U, .TRUE., V, IERR, WORK)
!
IF(IERR/=0) WRITE (6, 2) IERR
2  FORMAT(' TROUBLE IERR=', I4)
DO J = 1, N
   WRITE (6, 6) SIGMA(J)
end do
WRITE (6, 7)
DO I = 1, M
   WRITE (6, 6) (U(I, J), J = 1, N)
end do
WRITE (6, 7)
DO I = 1, N
   WRITE (6, 6) (V(I, J), J = 1, N)
end do
6  FORMAT(3F10.6)
7  FORMAT(1H)
STOP
END