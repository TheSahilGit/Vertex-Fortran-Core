 module find
 
   contains
 
 
     subroutine find_element(arr_in, dim_in, element, indx_arr,  newdim)
 
       ! This subroutine will take an array and an element to check as input 
       ! And give back an array that contains index of the orginal array where 
       ! that element is. 
 
       real*8, allocatable, dimension(:) :: arr_in
       integer*4 :: dim_in, newdim
       real*8 :: element
       integer*4, dimension(:), allocatable :: indx_arr
       integer*4 :: cc, i
 
 
       cc = 0
 
       print*, 'dim_in= ', dim_in
 
       do i= 1, dim_in
         write(*,*)arr_in(i)
         if(element == arr_in(i))then
            cc = cc + 1
            indx_arr(cc) = i
          end if
       end do
 
       newdim = cc
 
 
     end subroutine find_element
 
 end module find
 

