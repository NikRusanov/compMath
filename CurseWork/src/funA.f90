!Function for const A
real function functionA(z)
    real z 
    functionA = ((1.0 - exp(-0.8*z))/(z*(1 + 1.6 * z)))
    return
end 
!Function for const B 
real  function functionB(x)
    real x
    functionB = 1 + x - exp(0.75*x)
    return
end
!===================== system =====================
        subroutine spring_vibration (t, w, yp)
            real t, w(2), yp(2)
            real q, k
            !TO DO input params  
            q = 4.0 
            k = 5.0 
            yp(1)=(-q/(4*k**2))*((1-k**2)*w(2)+2*k**2*w(2)**3)
            yp(2)=w(1)
            return
        end
program main 
external functionB, functionA, ZEROIN, spring_vibration
integer NOFUN,i,j


!vars for quanc
real                    :: resA,interval_a,interval_b,relerr,abserr,flag,errest,result

!vars for zeroin 
real                    ::resB,functionB,AX,BX,TOL,ZEROIN


!vars for decomp,solve
real                    ::   A(3,3),B(3), WORK(3),COND
integer                 :: NDIM, N, IPTV(3), In, Out 
character(*), parameter :: input_file= "../data/input.txt", output_file = "output.txt"



!vars for RFF45 
real w(2), q, k, t, tout, tfinal, tprint, rwork(15)
integer iwork(5), iflag, neqn





N = 3 
NDIM = 3 
open(file=input_file, newunit=In)
    read(In,*) (A(:,i),i=1,N)
    read(In,*) (B(i), i=1,N)
close (In)
    !calclulate A
    interval_a = 1.e-06 !! divide by zero 
    interval_b = 1.0
    relerr = 1.e-06
    abserr = 0.0
Out = 0 
open(file=output_file, newunit=Out)
    call quanc8(functionA,interval_a,interval_b,abserr,relerr,result,errest,NOFUN,flag)
    write(Out,1) result,errest,NOFUN,flag
    ! find A 
    resA = (result - 0.40874702)**4
    write (Out,2) resA
    

    !calculate B 
    AX  = 0.5
    BX  = 0.8 
    TOL = 1.0E-6

   resB = ZEROIN(AX, BX, functionB, TOL)
   resB = resB * 0.05452555
   write (Out, 3) resB



   !calculate system 
   write(Out,*) "params:"
   write (Out,*) "A is  "
   write(Out,4) (A(:,j),j=1,N) 
   
   write (Out,*) "B is  "
   write(Out,4) B

   !CALCULATE SYSTEM
   call decomp(NDIM,N,A,COND, IPTV,WORK)
   write(Out,5) COND
   call SOLVE(NDIM,N,A,B,IPTV)
   write(Out,7)
   write(Out,6) B(:)
! ======== B(1) = k B(2) = q B(3) = T========
! TODO rename B(3) !!!



!init vars for dif system 
neqn = 2 
w(1) = resB
w(2) = resA
t = 0.000001 
q = B(1)
k = B(2)
tfinal = 30
iflag = 1 
tout = t 
tprint = 0.5

!calculate RKF 
10 call RKF45(spring_vibration,neqn,w,t,tout,relerr,abserr,iflag,rwork,iwork)
        write (Out,11) t,w(1),w(2)
        go to (80,20,30,40,50,60,70,80),iflag
20 tout=tprint + t
        if(t.lt.tfinal) go to 10
        stop
30 write( Out,31)relerr,abserr
      go to 10
40 write (Out, 41)
      go to 10
50 abserr=0.1e-07
      write (Out, 31) relerr,abserr
      go to 10
60 relerr=relerr*10.0
      write (Out, 31) relerr,abserr
      iflag=2
      go to 10
70 print 71
      iflag=2
      go to 10
80 write (Out, 81)


close(Out)
      stop
!====================================================================================
!=============================OUTPUT FORMATS=========================================
!==================================================================================== 
    1 format(10x,'result=',e14.7,3x,'errest=',e12.5/11x,'NOFUN=',i8,11x,'flag=',f10.3)
    2 format(10x,'resultA = ',e14.5,3x)
    3 format (10x,'result B = ', e14.7,3x)
    4 format (3x,f10.3, f10.3, f10.3)
    5 format (5x,'COND = ', f12.5)
    6 format (5x, 3(4x, F10.7))
    7 format (14x, 'k',14x,'q',14x,'t')



    11 format(' t=',f10.2,2x,'w1=',f10.6,2x,'w2=',E14.8)
    31 format(' ГPAHИЦЫ ПOГPEШHOCTEЙ ИЗMEHEHЫ  '/' RELERR=',E10.3,2X,'ABSERR=',E10.3)
    41 format(' MHOГO ШAГOB ')
    71 format(' MHOГO BЫXOДOB ')
    81 format(' HEПPABИЛЬHЫЙ BЫЗOB ')
    end program
