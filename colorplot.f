      program pplot
C
      include 'dem.h'
C
      integer wp,i2,times
      real*8  ab,bc,cd,de,ef,fg,pi,n,zb,zd
     &     ,l,hi,jk,lm,time,time1,z1,z2
      CHARACTER title*5
C
      write(*,*)'divide n 1-5'
      write(*,*)'n?'
      read(*,*)n
      write(*,*)'time?'
      read(*,*)time
      write(*,*)'time step?'
      read(*,*)times
      time=time*1000

      z1=0.
      z2=0.

      open(unit = 3,err = 9000,status = 'old',file =
     &   'dat/0.000.dat')

      read(3,*) ts, tp, te, dt
      read(3,*) rho, mu
      read(3,*) e,  ew, nu
      read(3,*) ang, cf, muw
C
      read(3,*) iwall
      do 11 i = 1, iwall
         read(3,*)      j
         read(3,*)   xw(i),   yw(i),   zw(i)
         read(3,*) ewnx(i), ewny(i), ewnz(i)
         read(3,*) ewtx(i), ewty(i), ewtz(i)
         read(3,*) ewbx(i), ewby(i), ewbz(i)
 11   continue
C
      read(3,*)    upx,    upy,    upz
      read(3,*) dpartx, dparty, dpartz
      read(3,*) ip
C
      do 21 i = 1, ip
         read(3,*)      j ,    r(i)
         read(3,*)    x(i),    y(i),    z(i)
         read(3,*)   vx(i),   vy(i),   vz(i)
         read(3,*) omgx(i), omgy(i), omgz(i)
 21   continue
C
      read(3,*,end=310) wp,i,j,l,ab,bc,cd
     $                    ,de,ef,fg
 310  close(3)

	z1=z(1)
	z2=z(1)

      do 22 i = 1, ip
C　最低位置の粒子の判別
         if ( z(i) .lt. z1 )then
             z1=z(i)
         endif
C　最高位置の粒子の判別
         if ( z(i) .gt. z2 )then
             z2=z(i)
         endif
 22   continue

      zb=z2-z1
      zd=zb/n
      write(*,*)'最低位置粒子:',z1,'最高位置粒子:',z2
      write(*,*)'分割数:',n,'分割幅:',zd

      do 220 j = 1, n
	counth(j) = 0
 220  continue

      do 23 j=1,n
         do 24 i=1,ip
            if ((z(i).ge.z1+zd*(j-1)).and.(z(i).le.z1+zd*j))then
                h(i)=j
		counth(j)=counth(j)+1
            endif
 24      continue
 23   continue

      do 221 j = 1, n
      write(*,*)'段数',j,'での粒子個数:',counth(j)
 221  continue

      do 30 i2=0,time,times

         open(unit = 10,err = 6060,status = 'old',file = 'count')
         close(unit = 10, status = 'delete' )
 6060    open(unit = 10,err = 9000,status = 'new',file = 'count')

         time1=i2/1000.

         write(10,1040) time1
         close(10)

         open(unit = 11,err = 9000,status = 'old',file = 'count')
         read(11,*) title
         close(11)

         OPEN (UNIT=1,ERR=50,STATUS='old',FILE='dat/'//title//'.dat')
C
         read(1,*) ts, tp, te, dt
         read(1,*) rho, mu
         read(1,*) e,  ew, nu
         read(1,*) ang, cf, muw
C
         read(1,*) iwall
         do 10 i = 1, iwall
            read(1,*)      j
            read(1,*)   xw(i),   yw(i),   zw(i)
            read(1,*) ewnx(i), ewny(i), ewnz(i)
            read(1,*) ewtx(i), ewty(i), ewtz(i)
            read(1,*) ewbx(i), ewby(i), ewbz(i)
 10      continue
C
         read(1,*)    upx,    upy,    upz
         read(1,*) dpartx, dparty, dpartz
         read(1,*) ip
C
         do 20 i = 1, ip
            read(1,*)      j ,    r(i)
            read(1,*)    x(i),    y(i),    z(i)
            read(1,*)   vx(i),   vy(i),   vz(i)
            read(1,*) omgx(i), omgy(i), omgz(i)
 20      continue
C
         read(1,*,end=300) wp,i,j,l,ab,bc,cd
     $                    ,de,ef,fg
 300     close(1)


         OPEN ( 2, ERR = 9900,STATUS = 'OLD',
     &    FILE ='plot2/'//title//'pc.dat')
         CLOSE( 2, STATUS = 'DELETE' )
 9900    OPEN ( 2, STATUS = 'NEW',
     &    FILE ='plot2/'//title//'pc.dat')
          write (2,*)'Dat',' x',' y',' z'
            do 55 i=1,ip
                         WRITE(2,*) h(i),x(I),y(I),z(I)
 55         continue

         CLOSE(2, STATUS = 'KEEP')

 50      continue
 30   continue


      stop

 1000   FORMAT(4E16.7)
 1040 format( F5.3 )
 9000 write(22,*) 'open error file.dat (unit=3)'

      end








