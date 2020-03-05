        subroutine orbit(t,y,yp)
            real t,y(2),yp(2)
			yp(1)=-430*y(1)-12000 * y(2)+exp(-10.0*t)
			yp(2)=y(1)+LOG(1.0 + 100*t*t)
            return
        end

        subroutine runge(t, h, y)
            external orbit
            real y(2), yp(2), zn1(2),zn13(2), h, temp(2),fn(2)
            real results(20, 2)

            
            call orbit(t,y,yp)
            temp = yp
            zn1 = y + h/3.0*yp
			call orbit(t + h/3.0,y,yp)
			zn13 = yp
			y = y + h/2.0*(-temp+3.0*zn13)
			
        end subroutine runge
        
#section main 
        external orbit
        real t,y(2), tfinal,tprint
        integer i
        i=1
        t=0.0000
        
        
        
        y(1)=3.0
        y(2)=-1.0

        tfinal=0.15
        tprint=0.00001
        h=0.001
		
   10   call runge(t, h, y)
		
        if(mod(i,1 )==0) then 
			print 11, t, y(1),y(2)
        endif
        t=h+t
        i=i+1
        if(t.lt.tfinal) go to 10
        print 11,t,y(1),y(2)
        11 format(' t=',f15.8,7x,'y1=',f15.8,7x,'y2=',f10.6)
        stop
        end
        

