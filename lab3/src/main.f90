subroutine orbit(t,y,yp)
	real t,y(2),yp(2)
	yp(1)=-430*y(1)-12000 * y(2)+exp(-10.0*t)
    yp(2)=y(1)+LOG(1.0 + 100*t*t)
      return
end

      external orbit
	  !enter args 
      real h,y(2),tprint,yp(2), tfinal,tout,t
      integer iwork(5),iflag,neqn 
      y(1)=3.0
      y(2)=-1.0
      t = 0.0000
      !x0 and x1 
      y(1)=3.0
      y(2)=-1.0
		!EPS

	  !final t last
      tfinal=0.15000
	  !шаг печати
      tprint=0.0075
      h = 0.0075
      10 print *,t
       call runge(orbit,h,t,y)
	   t = t + tprint
	  
	  if(t < tfinal) go to 10 
    
 end



subroutine runge(orbit,h,t, y) 
    external orbit 
	real h, y(2),yp(2)
	real k1(2)
	real t 
	real tmp(2)
    
     call orbit(t,y,yp) 
     k1 = y + (h/3)*yp 
	 tmp = (h/3)*yp
     call orbit(t+ h/3,y,yp)
     y = y + h/2*(-k1+3*(tmp + yp)) 
     print *,y
     return 
end
     
