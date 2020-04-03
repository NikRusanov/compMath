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
integer NOFUN,i


!vars for quanc
real resA,funictonA,interval_a,interval_b,rellerr,abserr,flag,errest,result

!vars for zeroin 
real resB,functionB,AX,BX,TOL,ZEROIN

    !calclulate A
    interval_a = 1.e-06 !! divide by zero 
    interval_b = 1.0
    rellerr = 1.e-06
    abserr = 0.0

    call quanc8(functionA,interval_a,interval_b,abserr,rellerr,result,errest,NOFUN,flag)
    print 1,result,errest,NOFUN,flag

    ! find A 
    resA = (result - 0.40874702)**4
    print 2,resA
    

    !calculate B 
    AX  = 0.5
    BX  = 0.8 
    TOL = 1.0E-7

   resB = ZEROIN(AX, BX, functionB, TOL)
   resB = resB * 0.05452555
   print 3,resB


    1 format(10x,'result=',e14.7,3x,'errest=',e12.5/11x,'NOFUN=',i8,11x,'flag=',f10.3)
    2 format(10x,'resultA = ',e14.5,3x)
    3 format (10x,'result B = ', e14.7,3x)
    end program
