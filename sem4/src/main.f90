      subroutine orbit(t,y,yp)
!меняем 4 на 2.
      real t,y(2),yp(2)
        ! у(1) = х1, у(2) =  х2
      yp(1)=-430*y(1)-12000 * y(2)+exp(-10.0*t)
      yp(2)=y(1)+LOG(1.0 + 100*t*t)

      return
      end

      external orbit
        !enter args 
      real t,y(2),tout,relerr,abserr, tfinal,tprint,ecc,work(27)
      integer iwork(5),iflag,neqn
      neqn=2
      !t0
      t=0.0000
      !x0 and x1 
      y(1)=3.0
      y(2)=-1.0
		!EPS
      relerr=1e-04
      abserr=0.0
	  !final t last
      tfinal=0.15000
	  !шаг печати
      tprint=0.0075
      iflag=1 
      tout=t
   10 call rkf45(orbit,neqn,y,t,tout,relerr,abserr, iflag,work,iwork)
      print 11,t,y(1),y(2), iflag
      go to (80,20,30,40,50,60,70,80),iflag
      !call rk if t final > current t 
   20 tout = tprint + t
      if(tout < tfinal) go to 10
      stop
   30 print 31,relerr,abserr
      go to 10
   40 print 41
      go to 10
   50 abserr=0.1e-07
      print 31,relerr,abserr
      go to 10
   60 relerr=relerr*10.0
      print 31,relerr,abserr
      iflag=2
      go to 10
   70 print 71
      iflag=2
      go to 10
   80 print 81
      stop
   11 format(' t=',f10.6,2x,'y1=',f10.6,2x,'y2=',f10.6,' FLAG=',I2)
   31 format(' ГPAHИЦЫ ПOГPEШHOCTEЙ ИЗMEHEHЫ  '/' RELERR=',E10.3,2X,'ABSERR=',E10.3)
   41 format(' MHOГO ШAГOB ')
   71 format(' MHOГO BЫXOДOB ')
   81 format(' HEПPABИЛЬHЫЙ BЫЗOB ')
      end
