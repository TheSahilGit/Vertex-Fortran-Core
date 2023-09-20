subroutine ForcePeriodic(L,v,inn,num,fxx,fyy)
  implicit none
  integer*8 :: i,j, ii,nn, L
!  integer*8, parameter :: N = 1760
  integer*8, dimension(1:900), intent(in):: num
  real*8, dimension(1:2,1:1760), intent(in) :: v
  integer*8, dimension(1:10,1:900), intent(in) :: inn
  real*8, dimension(1:1760), intent(inout) :: fxx, fyy
!  real*8, dimension(1:1760), intent(out):: fxxP, fyyP
  real*8, allocatable, dimension(:,:) :: temp_fx, temp_fy



!  open(125, file='force.dat', status='old')
!  open(126, file='forcePeriodic.dat', status='unknown')
!  open(11, file='num.dat',status='old')
!  open(12, file='v.dat', status='old')
!  open(13, file='inn.dat', status='old')
!
!  do i=1,1760
!    read(125,*)fxx(i),fyy(i)
!  end do
!
!  read(11,*)num
!  read(12,*)v
!  read(13,*)inn
!
!
!  close(11)
!  close(12)
!  close(13)
!  close(125)
!
!  L = 10


  allocate(temp_fx(10,L),temp_fy(10,L))
  
!----------------Right Panel-------------------------  
  temp_fx = 0.0d0
  temp_fy = 0.0d0
  
  ii = 0
  do i = L*L-L+1,L*L,1
    ii = ii + 1
    nn = num(i)
    do j=1,nn
      temp_fx(j,ii)=fxx(inn(j,i))
      temp_fy(j,ii)=fyy(inn(j,i))
    end do
  end do



  ii = 0
  do i = 3*L*L-L+1,3*L*L
    ii = ii + 1
    nn = num(i)
    do j=1,nn
      fxx(inn(j,i))= temp_fx(j,ii)
      fyy(inn(j,i))= temp_fy(j,ii)
    end do
  end do

!  write(*,*)fxx

!--------------Left Panel---------------------------  

  temp_fx = 0.0d0
  temp_fy = 0.0d0

  ii = 0
  do i = 1,L
    ii = ii + 1
    nn = num(i)
    do j=1,nn
      temp_fx(j,ii)=fxx(inn(j,i))
      temp_fy(j,ii)=fyy(inn(j,i))
    end do
  end do


  ii = 0
  do i = L*L+1,L*L+L
    ii = ii + 1
    nn = num(i)
    do j=1,nn
      fxx(inn(j,i))= temp_fx(j,ii)
      fyy(inn(j,i))= temp_fy(j,ii)
    end do
  end do



!--------------Top Panel---------------------------  

  temp_fx = 0.0d0
  temp_fy = 0.0d0

  ii = 0
  do i = L,L*L,L
    ii = ii + 1
    nn = num(i)
    do j=1,nn
      temp_fx(j,ii)=fxx(inn(j,i))
      temp_fy(j,ii)=fyy(inn(j,i))
    end do
  end do


  ii = 0
  do i = 4*L*L+L,5*L*L,L
    ii = ii + 1
    nn = num(i)
    do j=1,nn
      fxx(inn(j,i))= temp_fx(j,ii)
      fyy(inn(j,i))= temp_fy(j,ii)
    end do
  end do


!--------------Bottom Panel---------------------------  

  temp_fx = 0.0d0
  temp_fy = 0.0d0

  ii = 0
  do i = 1,L*L-L+1,L
    ii = ii + 1
    nn = num(i)
    do j=1,nn
      temp_fx(j,ii)=fxx(inn(j,i))
      temp_fy(j,ii)=fyy(inn(j,i))
    end do
  end do


  ii = 0
  do i = 3*L*L+1,4*L*L,L
    ii = ii + 1
    nn = num(i)
    do j=1,nn
      fxx(inn(j,i))= temp_fx(j,ii)
      fyy(inn(j,i))= temp_fy(j,ii)
    end do
  end do



!--------------Top Right---------------------------  

  temp_fx = 0.0d0
  temp_fy = 0.0d0

   i = 1
    ii =  1
    nn = num(i)
    do j=1,nn
      temp_fx(j,ii)=fxx(inn(j,i))
      temp_fy(j,ii)=fyy(inn(j,i))
    end do


   i = 5*L*L+1
    ii =  1
    nn = num(i)
    do j=1,nn
      fxx(inn(j,i))= temp_fx(j,ii)
      fyy(inn(j,i))= temp_fy(j,ii)
    end do


!--------------Top Left---------------------------  

  temp_fx = 0.0d0
  temp_fy = 0.0d0

   i = L*L-L+1
    ii =  1
    nn = num(i)
    do j=1,nn
      temp_fx(j,ii)=fxx(inn(j,i))
      temp_fy(j,ii)=fyy(inn(j,i))
    end do


   i = 9*L*L-L+1
    ii =  1
    nn = num(i)
    do j=1,nn
      fxx(inn(j,i))= temp_fx(j,ii)
      fyy(inn(j,i))= temp_fy(j,ii)
    end do



!--------------Bottom Left---------------------------  

  temp_fx = 0.0d0
  temp_fy = 0.0d0

   i = L*L
    ii =  1
    nn = num(i)
    do j=1,nn
      temp_fx(j,ii)=fxx(inn(j,i))
      temp_fy(j,ii)=fyy(inn(j,i))
    end do


   i = 7*L*L
    ii =  1
    nn = num(i)
    do j=1,nn
      fxx(inn(j,i))= temp_fx(j,ii)
      fyy(inn(j,i))= temp_fy(j,ii)
    end do



!--------------Bottom Right---------------------------  

  temp_fx = 0.0d0
  temp_fy = 0.0d0

   i = L
    ii =  1
    nn = num(i)
    do j=1,nn
      temp_fx(j,ii)=fxx(inn(j,i))
      temp_fy(j,ii)=fyy(inn(j,i))
    end do


  i = 7*L*L+L
    ii =  1
    nn = num(i)
    do j=1,nn
      fxx(inn(j,i))= temp_fx(j,ii)
      fyy(inn(j,i))= temp_fy(j,ii)
    end do


!  fxxP = fxx
!  fyyP = fyy


!  do i=1,1760
!    write(126,*)fxx(i),fyy(i)
!  end do
!
!  close(126)




end subroutine
