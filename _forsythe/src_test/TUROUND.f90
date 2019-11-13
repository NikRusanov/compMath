!     ИЛЛЮCTPИPУЮЩAЯ ПPOГPAMMA ДЛЯ URAND
!
IY = 0
DO I = 1, 10
   Z = URAND(IY)
   PRINT 1, Z
   1  FORMAT('    Z=', F10.7)
end do
STOP
END