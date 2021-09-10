c********0*********0*********0*********0*********0*********0*********
c Author: Paul Harasti (NRL)
c June 2016
c#####################################################################
      subroutine Lvl2_to_CompZ(rfname,radid
     :                         ,year,month,day,hour,minute,second
     :                         ,nthe)
c#####################################################################
c     Writes gridded radar data to a file
c
      implicit none
c
      include 'remap.inc'
c
      integer year
      integer month
      integer day
      integer hour
      integer minute
      integer second
      integer*2 nthe,elcount
      integer i,j,k,lines,m,q
      integer*2 azc

      real maxval,dummy
      
      character*100 rfname
      character*4  radid

      logical diag
 
      dummy=10.**(8.)
      diag=.false.
      if (diag) then
      print *,'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
      print *,'$          Message from radar_data_out       $'
      print *,'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
      print *,'$   rfname=',rfname(1:100)
      do i=1,nthe
      print *,'$   ref_gate1_range, the_num',ref_gate1_range(i),i
      enddo
      print *,'$   ref_gate_size(1)=',ref_gate_size(1)
      print *,'$   vel_gate1_range=',vel_gate1_range
      print *,'$   vel_gate_size=',vel_gate_size
      print *,'$   azmth_resolution(1)=',0.1*iazmth_resolution(1)
      print *,'$   nthe=',nthe
      print *,'$   nr=',nr
      print *,'$   year=',year
      endif

c Interpolate PPIs of variable start azimuths to standard azimuths

      do k=1,nthe
      azc=nphi(k)
      if (azc.gt.2) then
      call polar_to_polar(azc,k)
       do j=1,max_r
        do i=1,max_a
        if (gridpolar(j,i).lt.dummy) then
         ref_polr(j,i,k)=10.*alog10(gridpolar(j,i))
        else
         ref_polr(j,i,k)=refmis
        endif
        if (k.eq.1) compref_polr(j,i)=refmis
        enddo
       enddo
       elcount=k
       endif
      enddo

c calculate composite reflectivity

      do j=1,max_r
       do i=1,max_a
        maxval=refmis
        do k=1,elcount
         if (ref_polr(j,i,k).gt.maxval)
     +  maxval=ref_polr(j,i,k)
        enddo
        compref_polr(j,i)=maxval
       enddo
      enddo

c#######################################################################
c
c     Write out data
c
c#######################################################################
c

      open(11,file=rfname,form='formatted')
      write(11,110) radid 
      write(11,120) year,month,day,hour,minute,second
      write(11,130) max_r,real(ref_gate_size(1))/1000.,
     &ref_gate1_range(1)/1000.
         do 31 j=1,max_a
            lines=max_r/6
c           read full lines
            do 41 k=0,lines-1
               write(11,160) (compref_polr(k*6+m,j),m=1,6)
41          continue
            q=int(amod(real(max_r),6.))
            if (q.ne.0) write(11,160) (compref_polr(k*6+m,j),m=1,q)
31       continue

      close(11)

110   format(a4)
120   format(i4,5(1x,i2))
130   format(i3,1x,f6.3,1x,f6.3)
160   format(f6.2,5(1x,f6.2))

      RETURN
      END
