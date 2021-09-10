c
c
c#####################################################################
      subroutine uf_print_date_time(yyyymmddhhmmss,uyear,umonth
     &                          ,uday,uhour,uminute,usecond)
c#####################################################################
c********0*********0*********0*********0*********0*********0*********
c
      implicit none
c
c********0*********0*********0*********0*********0*********0*********
      integer*2 uyear
      integer*2 umonth
      integer*2 uday
      integer*2 uhour
      integer*2 uminute
      integer*2 usecond

      character*14 yyyymmddhhmmss
c********0*********0*********0*********0*********0*********0*********
      read(yyyymmddhhmmss(3:4),70)uyear
      read(yyyymmddhhmmss(5:6),70)umonth
   70 format(i2)
      read(yyyymmddhhmmss(7:8),70)uday
      read(yyyymmddhhmmss(9:10),70)uhour
      read(yyyymmddhhmmss(11:12),70)uminute
      read(yyyymmddhhmmss(13:14),70)usecond
c     print *,'data year=',data_year
c     print *,'data month=',data_month
c     print *,'data day=',data_day
c     print *,'data hour=',data_hour
c     print *,'data minute=',data_minute
c     print *,'data second=',data_second
c********0*********0*********0*********0*********0*********0*********
      return
      end
