!Function for A
real function functionA(z)
    real z 
    functionA = ((1.0 - exp(-0.8*z))/(z*(1 + 1.6 * z)))
    return
end 
!Function for B 
real  function functionB(x)
    real x
    functionB = 1 + x - exp(0.75*x)
    return
end



program main 
external functionB, functionA, ZEROIN
integer NOFUN,i,j


!vars for quanc
real                    :: resA,interval_a,interval_b,rellerr,abserr,flag,errest,result

!vars for zeroin 
real                    ::resB,functionB,AX,BX,TOL,ZEROIN


!vars for decomp,solve
real                    ::   A(3,3),B(3), WORK(3),COND
integer                 :: NDIM, N, IPTV(3), In, Out 
character(*), parameter :: input_file= "../data/input.txt", output_file = "output.txt"
N = 3 
NDIM = 3 
open(file=input_file, newunit=In)
    read(In,*) (A(:,i),i=1,N)
    read(In,*) (B(i), i=1,N)
close (In)
    !calclulate A
    interval_a = 1.e-06 !! divide by zero 
    interval_b = 1.0
    rellerr = 1.e-06
    abserr = 0.0
Out = 0 
open(file=output_file, newunit=Out)
    call quanc8(functionA,interval_a,interval_b,abserr,rellerr,result,errest,NOFUN,flag)
    write(Out,1) result,errest,NOFUN,flag
    ! find A 
    resA = (result - 0.40874702)**4
    write (Out,2) resA
    

    !calculate B 
    AX  = 0.5
    BX  = 0.8 
    TOL = 1.0E-7

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
close(Out)
   
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
    end program
