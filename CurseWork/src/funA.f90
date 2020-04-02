real function fun(z)
    real z 
    fun = (1.0 - exp(-0.8*z))/(z*(1.6*z))
    return
end 

external fun
integer NOFUN,i
real fun, A, B, rellerr, abserr, result, errest, flag
real points(11)
real con
con = -0.40874702       
a = 0.0
b = 1.0
rellerr = 1.e-06
abserr = 0.0
do i=1,10
    call quanc8(fun,a,b,abserr,rellerr,result,errest,NOFUN,flag)
    points(i) = 1.0 - result
enddo
    print 1,result,errest,NOFUN,flag
    stop
1 format(10x,'result=',e14.7,3x,'errest=',e12.5/11x,'NOFUN=',i8,11x,'flag=',f10.3)
end
