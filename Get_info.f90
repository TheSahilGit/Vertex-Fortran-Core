module Get_info
  contains

    subroutine get_edgelength(Lin,vin,numin,innin,djk_all)
      integer*4 :: kv, ig
           
      kv = 0
     do i=1:L*L
       nn = numin(ig)


       allocate(vx(1:nn), vy(1:nn))
       allocate(fx(1:nn), fy(1:nn))

       vx = v(1,inn(1:nn,i))
       vy = v(2,inn(1:nn,i))

       do j=1,nn
         jp = j+1
         jm = j-1
         if(j == nn)then
           jp = 1
         elseif(j==1)then
           jm = nn
         end if

         call CalculateDistance(vx(j),vy(j),vx(jp),vy(jp),d)
         djk_all(kv) = d
         kv = kv + 1

       end do
     end do


    

    end subroutine get_edgelength



end module Get_info
