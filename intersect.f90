! ****************************************************************************
! This program gives back a new array of common elements from two given array. 
! ****
!******************************************************************
!
! All of these array operations are defined for integer*4 arrays, because I need them like that. These are not general. So careful
! when use it in different data type.
!***********************************************************************
!
!********** Sahil Islam ( 08, Feb, 2023)
!
! ****************************************************************************


subroutine intersect(arr1,dim1,arr2,dim2,intarr,newdim)
    implicit none
    integer*4 :: i,j,k,co, cn
    integer*4 :: dim1,dim2 

!    real*8,dimension(dim1), intent(in) :: arr1
!    real*8,dimension(dim2), intent(in) :: arr2
!    real*8,dimension(co), intent(out) :: intarr

    integer*4,dimension(dim1) :: arr1
    integer*4,dimension(dim2):: arr2
    integer*4,dimension(co) :: intarr

    real*8 :: r1,r2
    integer*4 :: newdim

!    dim1 = 8
!    dim2 = 9

!    allocate(arr1(1:dim1),arr2(1:dim2))

    
!    do i=1,dim1
!       call random_number(r1)
!       arr1(i) = int(1+10*r1)
!    end do

!    do j=1,dim2
!        call random_number(r2)
!        arr2(j) = int(1+10*r2)
!    end do
!
!    arr1 = [3, 4, 1, 5, 2, 4, 2, 5]
!    arr2 = [1, 2, 5, 4, 7, 8, 3, 5, 9]



!    write(*,*)'arr1=',(arr1(i),i=1,dim1)
!    write(*,*)'arr2=',(arr2(i),i=1,dim2)

    co = 0
    do i=1,dim1
        do j=1,dim2
            if(arr1(i).eq.arr2(j))then
                co = co + 1
                !write(*,*)arr2(j)
            end if
        end do
    end do
    
!    allocate(intarr(1:co))

! This is a very weired way of doing it.
! But I'm not that good at Fortran yet.
! ... and it works :) 

    co=0 
    do i=1,dim1
        do j=1,dim2
            if(arr1(i).eq.arr2(j))then
                co = co + 1
                intarr(co)=arr1(i)
            end if
        end do
    end do
    
! Till now I have found the common elemetns between two arrays. 
! Now i have to delete the duplicate elements. 


!   write(*,*)'intarr=', (intarr(k),k=1,co)
   

   call unique(intarr,co,intarr,cn)

!   write(*,*)'intarr=', (intarr(k), k=1,cn)

   newdim = cn






end subroutine


 !******************************************************
 ! This code gives sorted unique elements of an 1D array
 !******************************************************
 
 
 subroutine unique(arr_in,dim_old,arr_out,dim_new)
     implicit none
     integer*4 :: i,j,k
     integer*4, intent(in) :: dim_old
     integer*4, dimension(dim_old),intent(in) :: arr_in
     integer*4, dimension(dim_old) :: dum
     integer*4 :: nn
     integer*4, intent(out) :: dim_new
     integer*4, dimension(nn), intent(out) :: arr_out
 
     dum = arr_in
     nn = dim_old
 
     do i=1,nn-1
        j = i+1
        do while(j<=nn)
            if(dum(i)==dum(j))then
                do k=j,nn
                    dum(k)=dum(k+1)
                end do
                nn = nn - 1
                j = j-1
            end if
            j = j+1
        end do
     end do
 
 
     call bubblesort(dum,nn,dum)
 
     arr_out = dum
     dim_new = nn
 
 end subroutine

!************************************************
!**** Bubblesort *******************************

 subroutine bubblesort(a_in,n,a_out)
 
     implicit none
     integer*4 :: i,j,k
     integer*4, intent(in):: n
     integer*4, dimension(n), intent(in):: a_in
     integer*4, dimension(n), intent(out) :: a_out
     integer*4 :: b,c
 
     a_out = a_in
 
     do i=1,n
         do j=1,n-1
             b = a_out(j)
             c = a_out(j+1)
             if(a_out(j)>a_out(j+1))then
                 a_out(j) = c
                 a_out(j+1) = b
                 !write(*,*)b
             end if
         end do
     end do
 
 
 end subroutine



