      subroutine byte_swap_r4(data,max)
c********0*********0*********0*********0*********0*********0*********
c
c Byte swap real*4 data
c    
c     Paul Harasti                             November 2008 
c
c********0*********0*********0*********0*********0*********0*********
c
      implicit none
c
      integer max
      integer n

      real*4 data(max)
      real*4 r4
      integer*1 i1(4)
      integer*1 byte
      equivalence (i1(1),r4)
c
c********0*********0*********0*********0*********0*********0*********
c
      do n=1,max
         r4=data(n)
         byte=i1(2)
         i1(2)=i1(3)
         i1(3)=byte
         byte=i1(4)
         i1(4)=i1(1)
         i1(1)=byte
         data(n)=r4
      enddo
c
c********0*********0*********0*********0*********0*********0*********
c
      return
      end
      
