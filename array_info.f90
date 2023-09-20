module array_info
!  use allocation

  
  !    implicit none

!    save
!
     integer*4 :: ip,jp,kp
!    integer*4 ::  dim1, dim2
!    integer*4, dimension(:), allocatable :: arry1, arry2, arry
!    integer*4, dimension(:), allocatable :: aa, bb
!    integer*4 :: a,b,c, cc
!
! 
!
!    contains
!
!    subroutine allocate_arrays
!        implicit none
!        n = 4
!        dim1 = 4
!        dim2 = 7
!        allocate(aa(n), bb(n))
!        allocate(arry1(dim1), arry2(dim2),arry(dim2))
!    end subroutine allocate_arrays

contains



  subroutine neighbor_data(arr2d, arr2d_dim1, arr2d_dim2, dim_arr, v_arr)
    implicit none
    integer*4 :: ip,jp,kp
    integer*4, dimension(:,:), allocatable :: arr2d, v_arr
    integer*4, dimension(:), allocatable :: dim_arr
    integer*4, dimension(:), allocatable :: arr_int
    integer*4 :: arr2d_dim1, arr2d_dim2, newdimInter 
    integer*4, dimension(:), allocatable :: temp_arr1, temp_arr2

    allocate(arr_int(arr2d_dim1))



    do ip = 1, arr2d_dim2
      do jp = 1, arr2d_dim2
        v_arr(ip,jp) = 0
      end do
    end do

    do ip = 1, arr2d_dim2
      do jp = ip+1, arr2d_dim2
        allocate(temp_arr1(dim_arr(ip)), temp_arr2(dim_arr(jp)))

        do kp = 1, dim_arr(ip)
          temp_arr1(kp) = arr2d(kp,ip)
        end do
        do kp = 1, dim_arr(jp)
          temp_arr2(kp) = arr2d(kp,jp)
        end do

        call intersect(temp_arr1, dim_arr(ip), temp_arr2, dim_arr(jp), arr_int, newdimInter)
        if(newdimInter>1)then
          v_arr(ip,jp) = 1
          v_arr(jp,ip) = 1
        end if

        deallocate(temp_arr1, temp_arr2)
      
      end do
    end do
      


end subroutine neighbor_data



subroutine intersect(arr1,dim1,arr2,dim2,intarr,newdim)
    implicit none
    integer*4, dimension(:), allocatable :: arr1,arr2,intarr
    integer*4 ::  co, cn
    integer*4 :: dim1, dim2, newdim
    real*8 :: r1,r2


    co=0 
    do ip=1,dim1
        do jp=1,dim2
            if(arr1(ip).eq.arr2(jp))then
                co = co + 1
                intarr(co)=arr1(ip)
            end if
        end do
    end do
    
! Till now I have found the common elemetns between two arrays. 
! Now i have to delete the duplicate elements. 


!   write(*,*)'intarr=', (intarr(k),k=1,co)
   

   call unique(intarr,co,intarr,cn)

!   write(*,*)'intarr=', (intarr(k), k=1,cn)

   newdim = cn


end subroutine intersect




subroutine unique(arr_in,dim_old,arr_out,dim_new)
     implicit none
     integer*4 :: dim_old
     integer*4, dimension(:), allocatable :: arr_in, arr_out
     integer*4, dimension(:), allocatable :: dum
     integer*4 :: nnp
     integer*4, intent(out) :: dim_new

     allocate(dum(dim_old))
 
     dum = arr_in
     nnp = dim_old
 
    do ip = 1,nnp-1
        jp = ip+1
        do while(jp<=nnp)
            if(dum(ip)==dum(jp))then
                do kp=jp,nnp-1
                    dum(kp) = dum(kp+1)
                end do
                nnp = nnp - 1
                jp = jp-1
            end if
            jp = jp+1
        end do
    end do

!    write(*,*)nn
 
 
    call bubblesort(dum,nnp,dum)

     arr_out = dum
     dim_new = nnp
 
 end subroutine
    




    subroutine bubblesort(a_in, np, a_out)
      implicit none
      integer*4, dimension(:), allocatable :: a_in, a_out
      integer*4 :: np
      integer*4 :: b, c

      a_out = a_in
    
        do ip=1,np
            do jp=1,np-1
                b = a_out(jp)
                c = a_out(jp+1)
                if(a_out(jp)>a_out(jp+1))then
                    a_out(jp) = c
                    a_out(jp+1) = b
                    !write(*,*)b
                end if
            end do
        end do
    
    
    end subroutine

    subroutine find_element(arr_in, dim_in, element, indx_arr,  newdim)

        ! This subroutine will take an array and an element to check as input
        ! And give back an array that contains index of the orginal array where
        ! that element is.

        integer*4, allocatable, dimension(:) :: arr_in
        integer*4 :: dim_in, newdim
        integer*4 :: element
        integer*4, dimension(:), allocatable :: indx_arr
        integer*4 :: cc, i


        cc = 0

!        print*, 'dim_in= ', dim_in

        do i= 1, dim_in
!          write(*,*)arr_in(i)
          if(element == arr_in(i))then
             cc = cc + 1
             indx_arr(cc) = i
           end if
        end do

        newdim = cc


    end subroutine find_element






end module array_info
