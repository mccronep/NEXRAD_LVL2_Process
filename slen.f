c#####################################################################
      subroutine slen (cstr,lenc)
c#####################################################################
c
c********0*********0*********0*********0*********0*********0*********
      implicit none
c********0*********0*********0*********0*********0*********0*********

      character*(*) cstr
      character*1 tab, carriage_return, linefeed
      integer maxlen, lenc, i

      tab=char(9)
      linefeed=char(10)
      carriage_return=char(13)
c
c-----------------------------------------------------------------
c  Get the size of character string
c-----------------------------------------------------------------
c
      maxlen=len(cstr)

      lenc= 0
      do 10 i=1,maxlen
        if ( (cstr(i:i).eq.' ') .or. (cstr(i:i).eq.tab) .or.
     &  (cstr(i:i).eq.carriage_return) .or. (cstr(i:i).eq.linefeed) ) 
     &  return
        lenc= i
 10   continue
c-----------------------------------------------------------------
      return
      end
