**************************************************
      subroutine ave(temp1,temp2,diff,totdiff,temp,dummy)
 
c     implicit none
      real diff,totdiff,temp1,temp2,temp,dummy
      if(abs(temp1-dummy).ge.0.01) then
        if(abs(temp2-dummy).ge.0.01) then
          if(abs(totdiff).lt.0.00001) then
            print*,"Error in Ave - NaN" 
            temp=dummy
            return
           endif
          temp=(temp1*(totdiff-diff)+temp2*diff)/totdiff
        else
          temp=temp1
        endif
      elseif(abs(temp2-dummy).ge.0.01) then
        temp=temp2
      else
        temp=dummy
      endif  
      return
      end    
**************************************************
