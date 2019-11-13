program ex01
        use Environment

   implicit none

   character(*), parameter :: output_file = "output.txt"
   integer(I_)             :: Out = 0, i, j, k
   integer, parameter      :: NDIM = 8, N = 8
   integer                 :: IPVT(N)
   real                    :: A(NDIM, N), inrase(NDIM,N), C(N, NDIM), R(NDIM, N),E(NDIM,N) ,WORK(N), COND, CONDP1, NORM,&
      P(5) = (/1.0, 0.1, 0.01, 0.0001, 0.000001/)
         A = reshape([ &
            0., -5., -8., -5., 8., 1., 3., 0., &
            -5., 0., -5., -5., -1., -6., 4., 6., &
            -8., -5., -17., 5., -2., 4., -1., -4., &
            -5., -5., 5., -15., -5., 7., 2., 6., &
             8., -1., -2., -5., -3., -6., -4., -3., &
             1., -6., 4., 7., -6., 15., -8., -5., &
             3., 4., -1., 2., -4., -8., -5., 5., &
             0., 6., -4., 6., -3., -5., 5., 1.&
         ], [NDIM, N])
   open (file=output_file, encoding=E_, newunit=Out)

        do k = 1 , 5 
            write(Out,"('=============================')")
            write(Out, "('p = ', f12.6)") P(k)
            write(Out,"('=============================')")
            A(1,1) = P(k) + 12.0    
            inrase = 0 
            DO i = 1,N 
                inrase(i,i) = 1.0 
            END DO 
            E = transpose(inrase)
            write ( Out, 101) ((A(I,J),J=1,N),I=1,N)
           ! copy A to C 
           C = transpose(A)          
            call DECOMP(NDIM, N, A, COND, IPVT, WORK)
         !calcule obr A 
            DO i = 1 ,N
                CALL SOLVE(NDIM,N,A,inrase(i,1:N),IPVT)
            END DO
            write (Out,106 ) (inrase(i,:), i = 1, N) 
            R = E - transpose(matmul(inrase,C))  
            NORM = NORM2(R)
            write (Out,108) R

            write(Out,"('=============================')")
            write (Out,109) NORM
            write (Out, 102) COND
            write(Out,"('=============================')")
            !PROVERKA
            write(Out, 107) transpose(matmul(C,inrase))
    
          end do 
   close (Out)
   stop
   101 FORMAT(13X,'A',8(/5X,8F12.6))
   106 FORMAT(13X,'INRASE',8(/5X,8F12.6))
   107 FORMAT(13X,'PROVERKA A*INRASE',8(/5X,8F12.6))
   108 FORMAT(13X,'R=E-inrase*A',8(/5X,8F12.6))
   102 format(5X, 'COND=', E12.5)
   109 format(5X, 'NORM=', E12.6)
   103 format(5X, 'MATPИЦA KЛACCИФИЦИPУETCЯ KAK BЫPOЖДEHHAЯ')
end program ex01
