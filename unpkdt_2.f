c********0*********0*********0*********0*********0*********0*********
c********0*********0*********0*********0*********0*********0*********
c********0*********0*********0*********0*********0*********0*********
c-----------------------------------------------------------------
c
c      unpkdt_2   unpacks a 2-byte integer date (jjj) and and a 4-byte
c               integer time (ihms) and returns year, month, day, hour,
c               min, and sec (ymdhms).  Variable "ierr" return a 0 if
c               no problems occurred, and a 1 otherwise.  This routine
c               assumes that the julian date value is computed from
c               jan. 1, 1970.
c
c               The calling program must define the input record fields
c               as integer*2 and/or integer*4 before passing the
c               variables.  A variable must also be defined to receive
c               the 14-byte returned date/time.
c-------------------------------------------------------------------
c
      subroutine unpkdt_2 (jjj, ihms, ymdhms, ierr)
c
c-------------------------------------------------------------------
c  
c    jjj = 2-byte integer passed in containing julian date based on
c          01/01/70 start date.
c    ihms = 4-byte integer passed in containing time.
c    ymdhms = 14 character date/time returned (yyyymmddhrmnsc).
c    ierr = 0 if successful, 1 if error (integer)
c  
c-------------------------------------------------------------------
c  
      integer*2   jjj
      integer*4   ihms
      integer ierr
      character*14 ymdhms
      integer lpyr(0:99)
      integer   jjlo(0:99), jjhi(0:99), molo(12), mohi(12),
     &          molplo(12), molphi(12)
c  
      data lpyr /1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,
     &           1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,
     &           1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,
     &           1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,
     &           1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0/
c  
      data molo / 1,32,60, 91,121,152,182,213,244,274,305,335/
      data mohi /31,59,90,120,151,181,212,243,273,304,334,365/
c  
      data molplo / 1,32,61, 92,122,153,183,214,245,275,306,336/
      data molphi /31,60,91,121,152,182,213,244,274,305,335,366/
c  
      data jjlo
c       (years 2000 - 2009)
     &   /10958,11324,11689,12054,12419,12785,13150,13515,13880,14246,
c       (years 2010 - 2019)
     &    14611,14976,15341,15707,16072,16437,16802,17168,17533,17898,
c       (years 2020 - 2029)
     &    18263,18629,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2030 - 2039)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2040 - 2049)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2050 - 2059)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2060 - 2069)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2070 - 2079)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2080 - 2088, 1989)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0, 6941,
c       (years 1990 - 1999)
     &     7306, 7671, 8036, 8402, 8767, 9132, 9497, 9863,10228,10593/
c  
      data jjhi
c       (years 2000 - 2009)
     &   /11323,11688,12053,12418,12784,13149,13514,13879,14245,14610,
c       (years 2010 - 2019)
     &    14975,15340,15706,16071,16436,16801,17167,17532,17897,18262,
c       (years 2020 - 2029)
     &    18628,18993,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2030 - 2039)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2040 - 2049)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2050 - 2059)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2060 - 2069)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2070 - 2079)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
c       (years 2080 - 2088, 1989)
     &        0,    0,    0,    0,    0,    0,    0,    0,    0, 7305,
c       (years 1990 - 1999)
     &     7670, 8035, 8401, 8766, 9131, 9496, 9862,10227,10592,10957/
c  
c-------------------------------------------------------------------
 10   format('unpkdt jjj=',i5,'  ihms=',i10)
      ierr = 0
      iyr = 99
      imo = 99
      ida = 99
      ihr = 99
      imn = 99
      isc = 99

c     print *, "jjj = ", jjj
c     print *, "ihms = ", ihms
c  
c-----------------------------------------------------------------
c  Compute Hour, Minute, and Second first...
c  Remove milliseconds....not needed...
c-----------------------------------------------------------------
c  
      ihrmn = ihms / 60
      ihr   = ihrmn / 60
      imn   = ihrmn - ihr * 60
      isc   = ihms - (ihrmn * 60)
      if (ihr.eq.24.and.imn.eq.0.and.isc.eq.0) ihr=0
      if(ihr.lt.0 .or. ihr.gt.23 .or. imn.lt.0 .or. imn.gt.59 .or.
     &   isc.lt.0 .or. isc.gt.59) then
      write(*,5)ihms, ihr, imn, isc
 5    format(' ERROR in unpkdt2 - ihms=',i10,'  ihr=',i3,'  imn=',
     &         i3,'  isc=',i3)
      ierr = 1
      endif
c  
c-----------------------------------------------------------------
c  Search table of julian low and high values to get the year..
c-----------------------------------------------------------------
c  
      do 20 i = 0, 99
        if (jjj .ge. jjlo(i) .and. jjj .le. jjhi(i)) then
          if (i.ge.70) then
            iyr = i + 1900
            jyr = i
          else
            iyr = i + 2000
            jyr = i
          endif
          jjj2 = jjj - jjlo(i) + 1
          go to 40
        endif
 20   continue
c  
      write(6,30) jjj
 30   format(' SUBR. unpkdt unable to decode year, jjj=',i8,
     &       '   99/99/99 returned')
      ierr = 1
      go to 100

 40   continue
c  
c-----------------------------------------------------------------
c  Compute the month and day value....
c  Decide whether or not this is a leap year...
c-----------------------------------------------------------------
c  
      if (lpyr(jyr) .eq. 1) then
c
c-----------------------------------------------------------------
c  Its a leap year.....
c-----------------------------------------------------------------
c
        do 60 i = 1, 12
          if (jjj2 .le. molphi(i)) then
            imo = i
            ida = jjj2 - molplo(i) + 1
            go to 100
          endif
 60     continue
      else
c
c-----------------------------------------------------------------
c  Its not a leap year.....
c-----------------------------------------------------------------
c
        do 70 i = 1, 12
          if (jjj2 .le. mohi(i)) then
            imo = i
            ida = jjj2 - molo(i) + 1
            go to 100
          endif
 70     continue
      endif

      write(6,80) jjj, jyr
 80   format(' SUBR. unpkdt unable to decode mo-da, jjj=',i8,
     &       ',  yr=',i3,'   99/99 returned')
      ierr = 1

 100  continue
c  
c-----------------------------------------------------------------
c...Build character string of date and time....
c-----------------------------------------------------------------
c  
      write(ymdhms,'(i4.4,5i2.2)') iyr,imo,ida,ihr,imn,isc
      return

      end
